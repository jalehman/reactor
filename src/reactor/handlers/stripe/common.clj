(ns reactor.handlers.stripe.common
  (:require [blueprints.models.event :as event]
            [blueprints.models.member-license :as member-license]
            [blueprints.models.order :as order]
            [clojure.spec.alpha :as s]
            [ribbon.core :as ribbon]
            [ribbon.event :as re]
            [toolbelt.datomic :as td]
            [toolbelt.async :refer [<!!?]]))

(defn fetch-event
  "Fetch the event data from Stripe given a reactor event."
  [conn {event-id :event/id :as event}]
  (<!!? (if-some [managed (-> event event/metadata :managed-account)]
          (re/fetch conn event-id :managed-account managed)
          (re/fetch conn event-id))))

(s/fdef fetch-event
        :args (s/cat :conn ribbon/conn?
                     :event td/entity?)
        :ret map?)


(defn event-subject-id
  "Get the id of the event's subject; that is, the id of the Stripe entity that
  this event pertains to."
  [event]
  (get-in event [:data :object :id]))

(s/fdef event-subject-id
        :args (s/cat :event map?)
        :ret string?)


(defn subscription-type
  "Given a `stripe-event`, determine what kind of invoice it's attached to."
  [db subs-id]
  (cond
    (some? (member-license/by-subscription-id db subs-id)) :rent
    (some? (order/by-subscription-id db subs-id))          :service
    :otherwise                                             :unknown))
