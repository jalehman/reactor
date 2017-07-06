(ns reactor.handlers.stripe.invoice.common
  (:require [reactor.handlers.common :refer :all]
            [reactor.handlers.stripe.common :as common]
            [ribbon.event :as re]))

(defn subs-id
  "The id of the subscription that this invoice is for."
  [stripe-event]
  (-> stripe-event re/subject :subscription))


(defn invoice-type
  "Given a `stripe-event`, determine what kind of invoice it's attached to."
  [db stripe-event]
  (common/subscription-type db (subs-id stripe-event)))


(defn invoice-dispatch [deps event stripe-event]
  (invoice-type (->db deps) stripe-event))


(defn invoice-dashboard-url
  ([invoice-id]
   (format "https://dashboard.stripe.com/invoices/%s" invoice-id))
  ([managed-id invoice-id]
   (format "https://dashboard.stripe.com/%s/invoices/%s" managed-id invoice-id)))
