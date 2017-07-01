(ns reactor.fixtures
  (:require [datomic.api :as d]
            [blueprints.core :as bp]
            [clojure.test :refer :all]))


;; =============================================================================
;; Connection
;; =============================================================================


(def ^:dynamic *conn* nil)


(defn acquire-conn []
  (let [db-name (gensym)
        db-uri  (str "datomic:mem://" db-name)]
    (d/create-database db-uri)
    (let [conn (d/connect db-uri)]
      (bp/conform-db conn :db.part/starcity)
      conn)))


(defn release-conn [conn]
  (d/release conn))


(defmacro with-conn
  "Acquires a datomic connection and binds it locally to symbol while executing
  body. Ensures resource is released after body completes. If called in a
  dynamic context in which *resource* is already bound, reuses the existing
  resource and does not release it."
  [symbol & body]
  `(let [~symbol (or *conn* (acquire-conn))]
     (try ~@body
          (finally
            (when-not *conn*
              (release-conn ~symbol))))))


(defn conn-fixture
  "Fixture function to acquire a Datomic connection for all tests in a
  namespace."
  [test-fn]
  (with-conn r
    (binding [*conn* r]
      (test-fn))))
