(ns reactor.handlers.operations
  (:require [reactor.dispatch :as dispatch]
            [blueprints.models.event :as event]
            [blueprints.models.events :as events]
            [datomic.api :as d]
            [clj-time.core :as t]
            [clj-time.coerce :as c]
            [toolbelt.date :as date]))



;; ==============================================================================
;; Queries ======================================================================
;; ==============================================================================


(defn- licenses-without-transitions-ending-in-days
  "Find all the licenses that do not have transitions that end a number of `days` after date `t`."
  [db t days]
  (let [then  (c/to-date (t/plus (c/to-date-time t) (t/days days)))
        start (date/beginning-of-day then)
        end   (date/end-of-day then)]
    (->> (d/q
          '[:find [?l ...]
            :in $ ?start ?end
            :where
            [(missing? $ ?l :member-license/transition)]
            [?l :member-license/ends ?date]
            [(.after ^java.util.Date ?date ?start)]
            [(.before ^java.util.Date ?date ?end)]]
          db start end)
         (map (partial d/entity db)))))


;; ==============================================================================
;; Dispatches ===================================================================
;; ==============================================================================



(defmethod dispatch/job :operations/first-of-month
  [deps event {:keys [t] :as params}]
  (event/job :operations/passive-renewals {:params       {:t t}
                                           :triggered-by event}))


(defn- find-passive-renewal-licenses
  [db t]
  (d/q '[:find ?l
         :in $ ?month
         :where
         [?l :member-license/ends ?term-end]
         [(t/before? ?term-end (date/end-of-month ?month))]
         [(t/after? ?term-end (date/beginning-of-month ?month))]
         [(missing? $ ?l :member-license/transition)]]
       db t))


(defmethod dispatch/job :operations/passive-renewals
  [deps event {:keys [t] :as params}]
  (event/job :operations/active-renewals {:params       {:t t}
                                          :triggered-by event}))

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
