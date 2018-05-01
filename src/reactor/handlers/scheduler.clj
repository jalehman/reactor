(ns reactor.handlers.scheduler
  (:require [blueprints.models.events :as events]
            [clj-time.coerce :as c]
            [clj-time.core :as t]
            [datomic.api :as d]
            [reactor.dispatch :as dispatch]
            [reactor.handlers.common :refer :all]
            [teller.payment :as tpayment]
            [toolbelt.core :as tb]
            [toolbelt.date :as date]
            [toolbelt.datomic :as td]))

;; =============================================================================
;; Helpers
;; =============================================================================


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
  [teller now]
  (->> (tpayment/query teller {:payment-types [:payment.type/rent]
                               :statuses      [:payment.status/due]})
       (filter
        (fn [payment]
          (t/before? (c/to-date-time (tpayment/due payment)) (t/now))))))


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
  [teller now days]
  (let [now (c/to-date-time now)]
    (->> (tpayment/query teller {:payment-types [:payment.type/rent]
                                 :statuses      [:payment.status/due]})
         (filter
          (fn [payment]
            (let [due (c/to-date-time (tpayment/due payment))]
              (t/within? (t/interval now (t/plus now (t/days days))) due)))))))


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
  [deps event t]
  (let [payments (query-overdue-rent-payments (->teller deps) t)
        deposits (query-overdue-deposits (->db deps) t)]
    (->> (cond-> []
           (seq payments) (conj (events/alert-all-unpaid-rent (map td/id payments) t))
           (seq deposits) (conj (events/alert-unpaid-deposits deposits t))

           true
           (concat
            (map (comp #(events/alert-payment-due % t) td/id)
                 (rent-payments-due-in-days (->teller deps) t 1))
            (map #(events/alert-deposit-due % t) (deposits-due-in-days (->db deps) t 5))))
         (map #(tb/assoc-when % :event/triggered-by (td/id event))))))


;; =============================================================================
;; triggered daily
;; =============================================================================


(defmethod dispatch/job :scheduler/daily [deps event {as-of :as-of}]
  (assert (within-a-day? as-of) "stale job; not processing.")
  (daily-events deps event as-of))
