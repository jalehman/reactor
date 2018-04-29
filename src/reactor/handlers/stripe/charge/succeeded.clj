(ns reactor.handlers.stripe.charge.succeeded
  (:require [reactor.dispatch :as dispatch]
            [reactor.handlers.common :refer :all]
            [reactor.handlers.stripe.common :as common]
            [teller.event :as tevent]))

(defmethod dispatch/stripe :stripe.event.charge/succeeded [deps event _]
  (let [se (common/fetch-event (->teller deps) event)]
    (tevent/handle-stripe-event (->teller deps) se)))
