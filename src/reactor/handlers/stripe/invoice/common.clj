(ns reactor.handlers.stripe.invoice.common
  (:require [teller.payment :as tpayment]))

(defn subs-id
  "The id of the subscription that this invoice is for."
  [stripe-event]
  (-> stripe-event (get-in [:data :object]) :subscription))


(defn invoice-dispatch
  [deps event payment]
  (tpayment/type (tpayment/subscription payment)))
