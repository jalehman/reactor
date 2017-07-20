(ns reactor.handlers.stripe.charge.succeeded
  (:require [blueprints.models.charge :as charge]
            [blueprints.models.event :as event]
            [blueprints.models.order :as order]
            [blueprints.models.payment :as payment]
            [blueprints.models.rent-payment :as rent-payment]
            [reactor.dispatch :as dispatch]
            [reactor.handlers.common :refer :all]
            [reactor.handlers.stripe.common :as common]
            [ribbon.event :as re]
            [taoensso.timbre :as timbre]))

;; =============================================================================
;; Transactions
;; =============================================================================


(defn- charge? [e]
  (contains? e :charge/status))


(defmulti process-successful-charge
  "Using the charge's type, produce a transaction to update any entities (if
  any) that need to be updated in the db."
  (fn [deps ent event]
    (if (charge? ent)
      (charge/type (->db deps) ent)
      (payment/payment-for ent))))


(defmethod process-successful-charge :default [deps _ event]
  (timbre/warn :stripe.event.charge.succeeded/unknown
               {:uuid (event/uuid event)}))


(defmethod process-successful-charge :payment.for/deposit [deps payment event]
  ;; Nothing to do!
  [])


(defmethod process-successful-charge :rent [deps charge event]
  (let [py (rent-payment/by-charge (->db deps) charge)]
    [(rent-payment/set-paid py)]))


(defmethod process-successful-charge :payment.for/order [deps payment event]
  (let [order (order/by-payment (->db deps) payment)]
    (assert (#{:order.status/placed} (order/status order))
            "Invalid state; order has not been placed.")
    ;; this functionality is handled by the invoice events when we're dealing with invoices
    (when-not (payment/invoice? payment)
      [(order/is-charged order)])))     ; TODO: Add test for this datom


(defn- invoice-charge? [stripe-event]
  (-> stripe-event re/subject :invoice some?))


;; NOTE: The only reason we're checking if the entity is a payment or charge is
;; because rent payments have not yet been migrated to use payments.
(defmethod dispatch/stripe :stripe.event.charge/succeeded [deps event _]
  (let [se  (common/fetch-event (->stripe deps) event)
        sid (re/subject-id se)
        ent (or (charge/by-id (->db deps) sid)
                (payment/by-charge-id (->db deps) sid))]
    (if (charge? ent)
      (assert (not (charge/succeeded? ent)) "Charge has already succeeded; not processing.")
      (assert (not (payment/paid? ent)) "Payment has already succeeded; not processing."))
    (when-not (invoice-charge? se)
      (conj
       (process-successful-charge deps ent event)
       (if (charge? ent) (charge/succeeded ent) (payment/is-paid ent))))))
