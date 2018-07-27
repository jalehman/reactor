(ns reactor.handlers.stripe.balance.available
  (:require [reactor.dispatch :as dispatch]
            [reactor.handlers.common :refer :all]
            [reactor.handlers.stripe.common :as common]
            [teller.event :as tevent]))


(defmethod dispatch/stripe :stripe.event.balance/available [deps event _]
  (let [se (common/fetch-event (->teller deps) event)]
    (tevent/handle-stripe-event (->teller deps) se)
    nil))
