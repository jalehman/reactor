(ns user
  (:require [reactor.core]
            [reactor.config :as config :refer [config]]
            [reactor.datomic :refer [conn]]
            [reactor.seed :as seed]
            [clojure.spec.test :as stest]
            [clojure.tools.namespace.repl :refer [refresh]]
            [mount.core :as mount :refer [defstate]]
            [taoensso.timbre :as timbre]))

(timbre/refer-timbre)


(defn- in-memory-db?
  "There's a more robust way to do this, but it's not really necessary ATM."
  [uri]
  (clojure.string/starts-with? uri "datomic:mem"))


(defstate seeder
  :start (when (in-memory-db? (:uri (config/datomic config)))
           (timbre/debug "seeding dev database...")
           (seed/seed conn)))


(def start #(mount/start-with-args {:env :dev}))


(def stop mount/stop)


(defn go []
  (start)
  (stest/instrument)
  :ready)


(defn reset []
  (stop)
  (refresh :after 'user/go))
