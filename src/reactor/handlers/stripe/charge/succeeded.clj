(ns reactor.handlers.stripe.charge.succeeded
  (:require [blueprints.models.charge :as charge]
            [blueprints.models.event :as event]
            [blueprints.models.order :as order]
            [blueprints.models.payment :as payment]
            [reactor.dispatch :as dispatch]
            [reactor.handlers.common :refer :all]
            [reactor.handlers.stripe.common :as common]
            [ribbon.event :as re]
            [taoensso.timbre :as timbre]))

;; =============================================================================
;; Transactions
;; =============================================================================


(defmulti process-successful-charge
  "Using the charge's type, produce a transaction to update any entities (if
  any) that need to be updated in the db."
  (fn [deps payment event]
    (payment/payment-for payment)))


(defmethod process-successful-charge :default [deps _ event]
  (timbre/warn :stripe.event.charge.succeeded/unknown
               {:uuid (event/uuid event)}))


;; invoice handlers take care of this
(defmethod process-successful-charge :payment.for/deposit [deps payment event]
  [])


(defmethod process-successful-charge :payment.for/rent [deps payment event]
  ;; invoice handlers take care of this
  [])


(defmethod process-successful-charge :payment.for/order [deps payment event]
  (let [order (order/by-payment (->db deps) payment)]
    (assert (#{:order.status/placed} (order/status order))
            "Invalid state; order has not been placed.")
    ;; this functionality is handled by the invoice events when we're dealing with invoices
    (when-not (payment/invoice? payment)
      [(order/is-charged order)])))     ; TODO: Add test for this datom


(defn- invoice-charge? [stripe-event]
  (-> stripe-event re/subject :invoice some?))


(defmethod dispatch/stripe :stripe.event.charge/succeeded [deps event _]
  (let [se        (common/fetch-event (->stripe deps) event)
        charge-id (re/subject-id se)
        payment   (payment/by-charge-id (->db deps) charge-id)]
    (assert (not (payment/paid? payment)) "Payment has already succeeded; not processing.")
    (when-not (invoice-charge? se)
      (conj
       (process-successful-charge deps payment event)
       (payment/is-paid payment)))))
