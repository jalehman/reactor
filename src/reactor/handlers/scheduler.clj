(ns reactor.handlers.scheduler
  (:require [blueprints.models.events :as events]
            [clj-time.coerce :as c]
            [clj-time.core :as t]
            [datomic.api :as d]
            [reactor.dispatch :as dispatch]
            [reactor.handlers.common :refer :all]
            [toolbelt.core :as tb]
            [toolbelt.date :as date]
            [toolbelt.datomic :as td]))

;; =============================================================================
;; Helpers
;; =============================================================================


#_(defn days-from?
  "Is the supplied `date` a number of `days` away from `from`? `from` defaults to
  current time in 2-arity version."
  ([date days]
   (days-from? (java.util.Date.) date days))
  ([from date days]
   (let [from (c/to-date-time from)
         date (c/to-date-time date)]
     (and (t/within? (t/interval from (t/plus from (t/days days))) date)
          (not (t/within? (t/interval from (t/plus from (t/days (dec days)))) date))))))


(defn within-a-day?
  "Is `t` within a day of now?"
  [t]
  (let [t (c/to-date-time t)]
    (t/within? (t/interval t (t/plus t (t/days 1))) (t/now))))


(def ^:private within
  '[[(within [?date ?start ?end])
     [(.before ^java.util.Date ?date ?end)]
     [(.after ^java.util.Date ?date ?start)]]])


;; =============================================================================
;; Queries


(defn- query-overdue-rent-payments
  "All overdue rent payments as of `now`."
  [db now]
  (d/q '[:find [?p ...]
         :in $ ?now
         :where
         [?p :payment/due ?due]
         [?p :payment/status :payment.status/due]
         [?m :member-license/rent-payments ?p]
         [?m :member-license/status :member-license.status/active]
         [(.before ^java.util.Date ?due ?now)]]
       db now))


(defn- query-overdue-deposits
  "All overdue deposits as of `now`."
  [db now]
  (->> (d/q '[:find ?d ?amount (sum ?pamt)
              :in $ ?now
              :where
              [?d :deposit/account ?a]
              [?a :account/licenses ?m]
              [?m :member-license/status :member-license.status/active]
              [?d :deposit/due ?due]
              [(.before ^java.util.Date ?due ?now)]
              [?d :deposit/amount ?amount]
              [?d :deposit/payments ?p]
              (or [?p :payment/status :payment.status/paid]
                  [?p :payment/status :payment.status/pending])
              [?p :payment/amount ?pamt]]
            db now)
       (filter (fn [[d amount amount-paid]] (not= amount amount-paid)))
       (map first)))


(defn- rent-payments-due-in-days
  "Produce entity ids of all unpaid rent payments due in `days` from `now`."
  [db now days]
  (let [then  (c/to-date (t/plus (c/to-date-time now) (t/days days)))
        start (date/beginning-of-day then)
        end   (date/end-of-day then)]
    (d/q '[:find [?p ...]
           :in $ % ?start ?end
           :where
           [?p :payment/due ?due]
           [?p :payment/status :payment.status/due]
           [?m :member-license/rent-payments ?p]
           [?m :member-license/status :member-license.status/active]
           (within ?due ?start ?end)]
         db within start end)))


(defn- deposits-due-in-days
  "Produce entity ids of unpaid security deposits due in `days` from `now`."
  [db now days]
  (let [then  (c/to-date (t/plus (c/to-date-time now) (t/days days)))
        start (date/beginning-of-day then)
        end   (date/end-of-day then)]
    (->> (d/q '[:find ?d ?amount (sum ?pamt)
                :in $ % ?start ?end
                :where
                [?d :deposit/account ?a]
                [?a :account/licenses ?m]
                [?m :member-license/status :member-license.status/active]
                [?d :deposit/due ?due]
                (within ?due ?start ?end)
                [?d :deposit/amount ?amount]
                [?d :deposit/payments ?p]
                (or [?p :payment/status :payment.status/paid]
                    [?p :payment/status :payment.status/pending])
                [?p :payment/amount ?pamt]]
              db within start end)
         (filter (fn [[_ amount amount-paid]] (not= amount amount-paid)))
         (map first))))


(defn- daily-events
  "Produce a vector of all events that should be processed as of time `t`,
  triggered by `event`."
  [db event t]
  (let [payments (query-overdue-rent-payments db t)
        deposits (query-overdue-deposits db t)]
    (->> (cond-> []
           (not (empty? payments)) (conj (events/alert-all-unpaid-rent payments t))
           (not (empty? deposits)) (conj (events/alert-unpaid-deposits deposits t))

           true
           (concat
            (map #(events/alert-payment-due % t) (rent-payments-due-in-days db t 1))
            (map #(events/alert-deposit-due % t) (deposits-due-in-days db t 5))))
         (map #(tb/assoc-when % :event/triggered-by (td/id event))))))


;; =============================================================================
;; triggered daily
;; =============================================================================


(defmethod dispatch/job :scheduler/daily [deps event {as-of :as-of}]
  (assert (within-a-day? as-of) "stale job; not processing.")
  (daily-events (->db deps) event as-of))


;; =============================================================================
;; Playground
;; =============================================================================


(comment

  (let [t (java.util.Date.)]
    (when-let [deposits (unless-empty (query-overdue-deposits (d/db conn) t))]
      (d/transact-async conn [(events/alert-unpaid-deposits [(first deposits)] t)])))


  (let [t (java.util.Date.)]
    (when-let [payments (unless-empty (query-overdue-rent-payments (d/db conn) t))]
      (d/transact-async conn [(events/alert-all-unpaid-rent [(first payments)] t)])))


  (daily-events (d/db conn) {} (java.util.Date.))


  @(d/transact conn [(events/process-daily-tasks (java.util.Date.))])


  ;; make some rent payments overdue
  (->> (d/q '[:find ?p ?due
              :where
              [?p :payment/account _]
              [?p :payment/due ?due]]
            (d/db conn))
       (map (fn [[payment due]]
              [:db/add payment :payment/due (c/to-date (t/minus (c/from-date due) (t/days (rand-int 15))))]))
       (d/transact conn)
       deref)


  ;; make some rent payments due tomorrow
  (->> (d/q '[:find [?p ...]
              :where
              [?p :payment/account _]]
            (d/db conn))
       (map (fn [payment]
              [:db/add payment :payment/due (c/to-date (t/plus (t/now) (t/days (rand-int 3))))]))
       (d/transact conn)
       deref)


  ;; make some deposits overdue
  (->> (d/q '[:find ?d ?due
              :where
              [?d :deposit/account _]
              [?d :deposit/due ?due]]
            (d/db conn))
       (map (fn [[deposit due]]
              [:db/add deposit :deposit/due (c/to-date (t/minus (c/from-date due) (t/days (rand-int 60))))]))
       (d/transact conn)
       deref)


  ;; make some deposits due in five days
  (->> (d/q '[:find [?d ...]
              :where
              [?d :deposit/account _]
              [?d :deposit/due ?due]]
            (d/db conn))
       (map (fn [deposit]
              [:db/add deposit :deposit/due (c/to-date (t/plus (t/now) (t/days (rand-nth (range 4 7)))))]))
       (d/transact conn)
       deref)


  (require '[reactor.datomic :refer [conn]])


  )
