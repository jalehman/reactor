(ns reactor.handlers.stripe.invoice.common
  (:require [datomic.api :as d]
            [reactor.handlers.common :refer :all]
            [reactor.handlers.stripe.common :as common]
            [ribbon.event :as re]))

;; TODO: Can remove the rent-payment portion when I migrate that to use unified
;; payments.
(defn- invoice-id->subs-id [db invoice-id]
  (let [payment      (d/entity db [:stripe/invoice-id invoice-id])
        rent-payment (d/entity db [:rent-payment/invoice-id invoice-id])]
    (if (some? payment)
      (:stripe/subs-id (:order/_payments payment))
      (:member-license/subscription-id (:member-license/_rent-payments rent-payment)))))


(defn subs-id
  "The id of the subscription that this invoice is for."
  ([stripe-event]
   (-> stripe-event re/subject :subscription))
  ([db stripe-event]
   (if-let [sid (subs-id stripe-event)]
     sid
     (if-let [invoice-id (-> stripe-event re/subject :invoice)]
       (invoice-id->subs-id db invoice-id)
       (throw (ex-info "Cannot find subscription id!" {:event (:id stripe-event)
                                                       :type  (:type stripe-event)}))))))


(defn invoice-type
  "Given a `stripe-event`, determine what kind of invoice it's attached to."
  [db stripe-event]
  (common/subscription-type db (subs-id db stripe-event)))


(defn invoice-dispatch [deps event stripe-event]
  (invoice-type (->db deps) stripe-event))


(defn invoice-dashboard-url
  ([invoice-id]
   (format "https://dashboard.stripe.com/invoices/%s" invoice-id))
  ([managed-id invoice-id]
   (format "https://dashboard.stripe.com/%s/invoices/%s" managed-id invoice-id)))
