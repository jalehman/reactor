(ns reactor.scheduler
  (:require [datomic.api :as d]
            [hara.io.scheduler :as sch]
            [mount.core :refer [defstate]]
            [blueprints.models.events :as events]
            [reactor.datomic :refer [conn]]
            [taoensso.timbre :as timbre]
            [blueprints.models.payment :as payment]
            [clj-time.coerce :as c]
            [clj-time.core :as t]))


(defn- query-overdue-rent-payments [db now]
  (d/q '[:find [?p ...]
         :in $ ?now
         :where
         [?p :payment/due ?due]
         [?p :payment/status :payment.status/due]
         [?m :member-license/rent-payments ?p]
         [(.before ^java.util.Date ?due ?now)]]
       db now))


(defn- query-overdue-deposits [db now]
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


(defn- unless-empty [data]
  (when-not (empty? data) data))


(defn- create-scheduler [conn]
  (let [params {:conn conn}]
    (sch/scheduler
     {:create-rent-payments
      {:handler  (fn [t {conn :conn}]
                   (d/transact conn [(events/create-monthly-rent-payments t)]))
       ;; first of every month
       :params   params
       :schedule "0 0 0 * 1 * *"}

      :notify-unpaid-rent
      {:handler  (fn [t {conn :conn}]
                   (when-let [payments (unless-empty (query-overdue-rent-payments (d/db conn) t))]
                     (d/transact-async conn [(events/alert-all-unpaid-rent payments t)])))
       :params   params
       ;; 9am every day
       :schedule "0 0 9 * * * *"}

      :notify-unpaid-deposits
      {:handler  (fn [t {conn :conn}]
                   (when-let [deposits (unless-empty (query-overdue-deposits (d/db conn) t))]
                     (d/transact-async conn [(events/alert-unpaid-deposits deposits t)])))
       :params   params
       ;; 9am every day
       :schedule "0 0 9 * * * *"}}
     {}
     {:clock {:timezone "PST"}})))


(defstate scheduler
  :start (do
           (timbre/info ::starting)
           (sch/start! (create-scheduler conn)))
  :stop (do
          (timbre/info ::stopping)
          (sch/stop! scheduler)))


(comment

  (let [t1 #inst "2017-10-03"
        t2 #inst "2017-10-04"]
    (sch/simulate (sch/scheduler
                   {:print-task {:handler  (fn [t] (println "TIME:" t))
                                 :schedule "0 0 9 * * * *"}})
                  {:start t1 :end t2}))

  (let [t (java.util.Date.)]
    (when-let [deposits (unless-empty (query-overdue-deposits (d/db conn) t))]
      (d/transact-async conn [(events/alert-unpaid-deposits [(first deposits)] t)])))


  (let [t (java.util.Date.)]
    (when-let [payments (unless-empty (query-overdue-rent-payments (d/db conn) t))]
      (d/transact-async conn [(events/alert-all-unpaid-rent [(first payments)] t)])))


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


  )
