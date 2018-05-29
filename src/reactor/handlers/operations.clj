(ns reactor.handlers.operations
  (:require [reactor.dispatch :as dispatch]
            [blueprints.models.event :as event]
            [blueprints.models.events :as events]
            [datomic.api :as d]
            [clj-time.core :as t]
            [toolbelt.date :as date]))


(defmethod dispatch/job :operations/first-of-month
  [deps event {:keys [t] :as params}]
  (event/job :operations/passive-renewals {:params {:triggered-by event
                                                    :t t}}))


(defn- find-passive-renewal-licenses
  [db t]
  (d/q `[:find ?l
         :in $ ?month
         :where
         [?l :member-license/ends ?term-end]
         [(t/before? ?term-end (date/end-of-month ?month))]
         [(t/after? ?term-end (date/beginning-of-month ?month))]
         [(missing? $ ?l :member-license/transition)]]
       db t))

(defmethod dispatch/job :operations/passive-renewals
  [deps event {:keys [t] :as params}]
  (event/job :operations/active-renewals {:params {:triggered-by event
                                                   :t            t}}))

(defn- find-active-renewal-licenses
  [db t]
  (d/q '[:find ?l
         :in $ ?month
         :where
         [?l :member-license/ends ?term-end]
         [(t/before? ?term-end (date/end-of-month ?month))]
         [(t/after? ?term-end (date/beginning-of-month ?month))]
         [?l :member-license/transition ?transition]
         [?transition :license-transition/type :license-transition.type/move-out]]
       db t))

(defmethod dispatch/job :operations/active-renewals
  [deps event {:keys [t] :as params}]
  (events/create-monthly-rent-payments t))


;; there appears to already be a daily events job - see `reactor.handlers.scheduler`
(defmethod dispatch/job :operations/dailies
  [deps event params]
  ;;TODO - dispatch an event that will
  ;;   1. look for licenses ending on that day
  ;;   2. deactivate those licenses
  ;;   3. if any of the licenses have a "next license" attr, create/activate those new licenses
  )
