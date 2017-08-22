(ns reactor.handlers.stripe.invoice.updated
  (:require [blueprints.models.event :as event]
            [blueprints.models.member-license :as member-license]
            [blueprints.models.payment :as payment]
            [reactor.dispatch :as dispatch]
            [reactor.handlers.common :refer :all]
            [reactor.handlers.stripe.common :as common]
            [reactor.handlers.stripe.invoice.common :as ic]
            [ribbon.event :as re]
            [taoensso.timbre :as timbre]))

;; The "invoice.updated" event is issued after Stripe has attempted to charge
;; the customer. It's at this point that we'll first have a reference to a
;; Stripe charge id.


(defmulti invoice-updated ic/invoice-dispatch)


(defmethod invoice-updated :default [_ event stripe-event]
  (timbre/warn :stripe.event.invoice.updated/unknown
               {:uuid         (event/uuid event)
                :invoice      (re/subject-id stripe-event)
                :subscription (ic/subs-id stripe-event)}))


(defmethod invoice-updated :rent [deps event stripe-event]
  (let [invoice-id (re/subject-id stripe-event)
        license    (member-license/by-invoice-id (->db deps) invoice-id)
        payment    (payment/by-invoice-id (->db deps) invoice-id)
        charge-id  (:charge (re/subject stripe-event))]
    (when-not (some? (payment/charge-id payment))
      (assert (some? charge-id) "event has no charge id")
      (payment/add-charge payment charge-id))))


(defmethod invoice-updated :service [deps event stripe-event]
  (let [invoice-id (re/subject-id stripe-event)
        payment    (payment/by-invoice-id (->db deps) invoice-id)]
    (payment/add-charge payment (:charge (re/subject stripe-event)))))


(defmethod dispatch/stripe :stripe.event.invoice/updated [deps event _]
  (let [stripe-event (common/fetch-event (->stripe deps) event)]
    (invoice-updated deps event stripe-event)))
