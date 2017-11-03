(ns reactor.scheduler
  (:require [blueprints.models.events :as events]
            [datomic.api :as d]
            [hara.io.scheduler :as sch]
            [mount.core :refer [defstate]]
            [reactor.datomic :refer [conn]]
            [taoensso.timbre :as timbre]))

(defn- create-scheduler [conn]
  (let [params {:conn conn}]
    (sch/scheduler
     {:create-rent-payments
      {:handler  (fn [t {conn :conn}]
                   (d/transact-async conn [(events/create-monthly-rent-payments t)]))
       ;; first of every month
       :params   params
       :schedule "0 0 0 * 1 * *"}

      :daily-events
      {:handler  (fn [t {conn :conn}]
                   (d/transact-async conn [(events/process-daily-tasks t)]))
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

  )
