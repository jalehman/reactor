(ns reactor.handlers.stripe.charge.succeeded
  (:require [blueprints.models.event :as event]
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
  "Using the charge's type, produce a transaction to update any entities (if any)
  that need to be updated in the db."
  (fn [deps payment event]
    (payment/payment-for2 (->db deps) payment)))


(defmethod process-successful-charge :default [deps _ event]
  (timbre/warn :stripe.event.charge.succeeded/unknown
               {:uuid (event/uuid event)}))


(defmethod process-successful-charge :payment.for/deposit [deps payment event]
  [])


(defmethod process-successful-charge :payment.for/rent [deps payment event]
  [])


(defmethod process-successful-charge :payment.for/order [deps payment event]
  [[:db/add (:db/id payment) :payment/paid-on (java.util.Date.)]])


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
