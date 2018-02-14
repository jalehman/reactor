(ns reactor.handlers.newsletter
  (:require [reactor.config :as config :refer [config]]
            [reactor.dispatch :as dispatch]
            [reactor.handlers.common :refer :all]
            [hubspot.contact :as contact]))

(defmethod dispatch/job :newsletter/subscribe [deps event {email :email}]
  (when-let [key (config/hubspot-api-key config)]
    (let [c (clojure.core.async/chan 1)]
      (contact/create! email {} {:api-key key :out-ch c}))))
