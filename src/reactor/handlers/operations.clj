(ns reactor.handlers.operations
  (:require [blueprints.models.event :as event]
            [blueprints.models.events :as events]
            [blueprints.models.license-transition :as transition]
            [blueprints.models.member-license :as member-license]
            [clj-time.coerce :as c]
            [clj-time.core :as t]
            [datomic.api :as d]
            [reactor.dispatch :as dispatch]
            [reactor.handlers.common :refer :all]
            [teller.customer :as tcustomer]
            [teller.subscription :as tsubscription]
            [toolbelt.date :as date]
            [toolbelt.datomic :as td]))

;; ==============================================================================
;; Daily ========================================================================
;; ==============================================================================


(defmethod dispatch/job :ops/daily
  [deps event params]
  ;;TODO - dispatch an event that will
  ;;   1. look for licenses ending on that day
  ;;   2. deactivate those licenses
  ;;   3. if any of the licenses have a "next license" attr, create/activate those new licenses
  )

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
;; First of Month ===============================================================
;; ==============================================================================


(defmethod dispatch/job :ops/first-of-month
  [deps event {:keys [t] :as params}]
  (event/job ::passive-renewals {:params       {:t t}
                                 :triggered-by event}))


;; ==============================================================================
;; End of Month =================================================================
;; ==============================================================================


(defmethod dispatch/job :ops/end-of-month
  [deps event {:keys [t] :as params}]
  ;; do end of month shit
  (let [t      (c/to-date (t/plus (c/to-date-time t) (t/days 2)))
        period (date/beginning-of-month t)]
    (event/job ::deactivate-autopay-for-move-outs {:params       {:period period}
                                                   :triggered-by event})))


;; deactivate autopay ===========================================================


(defn- all-moveout-licenses [db]
  (->> (transition/by-type db :license-transition.type/move-out)
       (map transition/current-license)))


(defn- is-ending-within? [period license]
  (let [ends (member-license/ends license)
        tz   (member-license/time-zone license)
        from (date/beginning-of-month period tz)
        to   (date/end-of-month period tz)]
    (t/within? from to ends)))


(defn- moveouts-next-month
  [db period]
  (->> (all-moveout-licenses db)
       (filter (partial is-ending-within? period))))


(defn- autopay-subscription [teller customer]
  (first (tsubscription/query teller {:customers     [customer]
                                      :payment-types [:payment.type/rent]
                                      :canceled      false})))


(defn- autopay-on?
  [teller license]
  (let [account  (member-license/account license)
        customer (tcustomer/by-account (->teller deps) account)]
    (some? (autopay-subscription teller customer))))


(defmethod dispatch/job ::deactivate-autopay-for-move-outs
  [deps event {:keys [period] :as params}]
  ;; figure out who needs autopay deactivated
  (let [licenses (->> (moveouts-next-month (->db deps) period)
                      (filter (partial autopay-on? (->teller deps))))]
    ;; for each one, dispatch a deactivate autopay event
    (map
     (fn [license]
       (event/job ::deactivate-autopay {:params       {:license-id (td/id license)}
                                        :triggered-by event}))
     licenses)))


(defmethod dispatch/job ::deactivate-autopay
  [deps event {:keys [license-id] :as params}]
  (let [license  (d/entity (->db deps) license-id)
        account  (member-license/account license)
        customer (tcustomer/by-account (->teller deps) account)]
    (tsubscription/cancel! (autopay-subscription (->teller deps) customer))))


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


(defmethod dispatch/job ::passive-renewals
  [deps event {:keys [t] :as params}]
  (event/job ::active-renewals {:params                 {:t t}
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



(defmethod dispatch/job ::active-renewals
  [deps event {:keys [t] :as params}]
  (events/create-monthly-rent-payments t))
