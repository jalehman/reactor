(ns reactor.handlers.stripe.common
  (:require [blueprints.models.event :as event]
            [clojure.spec.alpha :as s]
            [stripe.event :as sevent]
            [teller.core :as teller]
            [toolbelt.core :as tb]
            [toolbelt.datomic :as td]))

(defn subject
  [event]
  (get-in event [:data :object]))


(defn fetch-event
  "Fetch the event data from Stripe given a reactor event."
  [teller {event-id :event/id :as event}]
  (sevent/fetch event-id
                (tb/assoc-when
                 {:token (teller/py teller)}
                 :account (-> event event/metadata :managed-account))))

(s/fdef fetch-event
        :args (s/cat :token teller/connection?
                     :event td/entity?)
        :ret map?)
