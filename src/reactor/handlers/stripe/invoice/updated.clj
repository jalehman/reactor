(ns reactor.handlers.stripe.invoice.updated
  (:require [reactor.dispatch :as dispatch]
            [reactor.handlers.common :refer :all]
            [reactor.handlers.stripe.common :as common]
            [teller.event :as tevent]))

(defmethod dispatch/stripe :stripe.event.invoice/updated [deps event _]
  (let [se (common/fetch-event (->teller deps) event)]
    (tevent/handle-stripe-event (->teller deps) se)
    nil))
