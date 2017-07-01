(ns reactor.handlers.stripe.common
  (:require [clojure.spec :as s]
            [reactor.models.event :as event]
            [ribbon
             [core :as ribbon]
             [event :as re]]
            [toolbelt
             [async :refer [<!!?]]
             [predicates :as p]]))

(defn fetch-event
  "Fetch the event data from Stripe given a reactor event."
  [conn {event-id :event/id :as event}]
  (<!!? (if-some [managed (-> event event/metadata :managed-account)]
          (re/fetch conn event-id :managed-account managed)
          (re/fetch conn event-id))))

(s/fdef fetch-event
        :args (s/cat :conn ribbon/conn?
                     :event p/entity?)
        :ret map?)


(defn event-subject-id
  "Get the id of the event's subject; that is, the id of the Stripe entity that
  this event pertains to."
  [event]
  (get-in event [:data :object :id]))

(s/fdef event-subject-id
        :args (s/cat :event map?)
        :ret string?)
