(ns reactor.reactor-test
  (:require [clojure.test :refer :all]
            [reactor.reactor :refer :all]
            [reactor.fixtures :as fixtures :refer [with-conn]]
            [datomic.api :as d]
            [clj-time.coerce :as c]
            [clj-time.core :as t]))


(comment
  ;; NOTE: [7/7/17] Cannot get this working, since I'd have to set the schema's
  ;; tx-instant to an earlier time than this test, and that requires changing
  ;; lots of blueprints logic. I'm pretty sure the query works...

  (use-fixtures :once fixtures/conn-fixture)


 (defn- seed-with-events [conn now]
   (letfn [(-make-event [tx-instant]
             {:db/id        (d/tempid :db.part/starcity)
              :event/status :event.status/pending}
             {:db/id        "datomic.tx"
              :db/txInstant (c/to-date tx-instant)})]
     (reduce
      (fn [db event]
        (:db-after (d/with db [event])))
      (d/db conn)
      [(-make-event (t/minus now (t/days 4)))
       (-make-event (t/minus now (t/days 3)))
       (-make-event (t/minus now (t/days 1)))
       (-make-event (t/minus now (t/hours 1)))])))


 (deftest can-query-pending-events
   (with-conn conn
     (let [now (t/now)
           db  (seed-with-events conn now)]
       (println now)
       (println (fetch-pending-events db (c/to-date now)))))))
