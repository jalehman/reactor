(ns reactor.handlers.newsletter
  (:require [reactor.dispatch :as dispatch]
            [reactor.handlers.common :refer :all]
            [reactor.services.weebly :as weebly]))

(defmethod dispatch/topicless :newsletter/subscribe [deps event {email :email}]
  (weebly/subscribe! (->weebly deps) email))
