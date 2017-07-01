(ns reactor.handlers.stripe.charge.succeeded
  (:require [blueprints.models
             [charge :as charge]
             [order :as order]
             [payment :as payment]
             [rent-payment :as rent-payment]
             [security-deposit :as deposit]]
            [reactor.dispatch :as dispatch]
            [reactor.handlers.common :refer :all]
            [reactor.handlers.stripe.common :as common]
            [reactor.models.event :as event]
            [taoensso.timbre :as timbre]
            [toolbelt.datomic :as td]))


;; =============================================================================
;; Notify Customer
;; =============================================================================


;; (defmulti notify-customer
;;   "Notify the customer of a successful charge."
;;   (fn [deps event params]
;;     (:type params)))


;; (defmethod dispatch/mail :stripe.event.charge.succeeded/notify-customer
;;   [deps event params]
;;   )


;; =============================================================================
;; Transactions
;; =============================================================================


(defmulti process-successful-charge
  "Using the charge's type, produce a transaction to update any entities (if
  any) that need to be updated in the db."
  (fn [deps charge event]
    (charge/type (->db deps) charge)))


(defmethod process-successful-charge :default [deps charge event]
  (timbre/warn :stripe.event.charge.succeeded/unknown
               {:uuid   (event/uuid event)
                :charge (charge/id charge)}))


(defmethod process-successful-charge :security-deposit [deps charge event]
  (let [deposit    (deposit/by-charge charge)
        new-amount (+ (deposit/amount-received deposit)
                      (int (charge/amount charge)))]
    [{:db/id                            (td/id deposit)
      :security-deposit/amount-received new-amount}
     #_(event/create :stripe.event.charge.succeeded/notify-customer
                     {:params       {:account-id (-> charge charge/account td/id)
                                     :charge-id  (td/id charge)
                                     :type       :security-deposit}
                      :triggered-by event
                      :topic        :mail})]))


(defmethod process-successful-charge :rent [deps charge event]
  (let [py (rent-payment/by-charge (->db deps) charge)]
    [(rent-payment/set-paid py)]))


(defmethod process-successful-charge :service [deps charge event]
  (let [payment (payment/by-charge-id (->db deps) (charge/id charge))
        order   (order/by-payment (->db deps) payment)]
    (assert (#{:order.status/placed} (order/status order))
            "Invalid state; order has not been placed.")
    ;; this functionality is handled by the invoice events when we're dealing with invoices
    (when-not (payment/invoice? payment)
      [(payment/is-paid payment)])))


(defmethod dispatch/stripe :stripe.event.charge/succeeded [deps event _]
  (let [se (common/fetch-event (->stripe deps) event)
        ch (charge/by-id (->db deps) (common/event-subject-id se))]
    (assert (not (charge/succeeded? ch))
            "Charge has already succeeded; not processing.")
    (concat
     (process-successful-charge deps ch event)
     [(charge/succeeded ch)])))
