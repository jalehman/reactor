(ns user
  (:require [blueprints.models
             [account :as account]
             [license :as license]
             [member-license :as member-license]
             [order :as order]
             [service :as service]]
            [clj-time
             [coerce :as c]
             [core :as t]]
            [clojure.spec.test :as stest]
            [clojure.tools.namespace.repl :refer [refresh]]
            [datomic.api :as d]
            [mount.core :as mount]
            [reactor.core :refer [conn]]
            [taoensso.timbre :as timbre]
            [reactor.models.event :as event]))

(timbre/refer-timbre)


(def start #(mount/start-with-args {:env :dev}))


(def stop mount/stop)


(defn go []
  (start)
  (stest/instrument)
  :ready)


(defn reset []
  (stop)
  (refresh :after 'user/go))
