(ns reactor.datomic
  (:require [blueprints.core :as blueprints]
            [clojure.string :as string]
            [datomic.api :as d]
            [mount.core :refer [defstate]]
            [reactor.config :as config]
            [taoensso.timbre :as timbre]))


(defn- new-connection [{:keys [uri partition] :as conf}]
  (timbre/info ::connecting {:uri (string/replace uri #"password.*" "")})
  (d/create-database uri)
  (let [conn (d/connect uri)]
    (blueprints/conform-db conn partition)
    conn))


(defn- disconnect [conn]
  (timbre/info ::disconnecting)
  (d/release conn))


(defstate conn
  :start (new-connection (config/datomic config/config))
  :stop  (disconnect conn))
