(ns reactor.handlers.stripe.common
  (:require [clojure.spec :as s]
            [blueprints.models.event :as event]
            [blueprints.models.member-license :as member-license]
            [blueprints.models.order :as order]
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


;; =============================================================================
;; new -- introducing in conjunction with `with-connect-account` . 112620171649
;; =============================================================================


(defn event->stripe
  "Synchronously fetch the Stripe event corresponding to this reactor `event`."
  [conn {event-id :event/id :as event}]
  (<!!? (re/fetch conn event-id)))

(s/fdef event->stripe
        :args (s/cat :conn ribbon/conn?
                     :event p/entity?)
        :ret map?)


(defn connect-account
  "The id of the Stripe Connect account, if present."
  [event]
  (-> event event/metadata :managed-account))

(s/fdef connect-account
        :args (s/cat :event p/entity?)
        :ret (s/or :nil nil? :connect-id string?))


;; =============================================================================



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
