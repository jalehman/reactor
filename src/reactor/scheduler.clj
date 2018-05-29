(ns reactor.scheduler
  (:require [blueprints.models.events :as events]
            [blueprints.models.event :as event]
            [datomic.api :as d]
            [hara.io.scheduler :as sch]
            [mount.core :refer [defstate]]
            [reactor.config :as config :refer [config]]
            [reactor.datomic :refer [conn]]
            [reactor.hubspot-sync :as hubspot-sync]
            [taoensso.timbre :as timbre]
            [toolbelt.core :as tb]))

(defn- create-scheduler [conn]
  (let [params {:conn conn}]
    (sch/scheduler
     (tb/assoc-when
      ;; process any license transitions this month, then create rent payments as appropro
      {:conduct-monthly-operations
       {:handler  (fn [t {conn :conn}]
                    (d/transact-async conn [(event/job :operations/first-of-month {:params {:t t}})]))
        ;; first of every month
        :params   params
        :schedule "0 0 0 * 1 * *"}

       :daily-events
       {:handler  (fn [t {conn :conn}]
                    (d/transact-async conn [(events/process-daily-tasks t)]))
        :params   params
        ;; 9am every day
        :schedule "0 0 9 * * * *"}}

      :hubspot-application-sync
      (when (config/production? config)
        {:handler  (fn [t {conn :conn}]
                     (hubspot-sync/sync-applications conn))
         :params   params
         :schedule "0 0 8-19 * * * *"}))
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

  )
