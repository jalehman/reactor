(ns reactor.handlers.operations-test
  (:require [blueprints.core :as blueprints]
            [blueprints.models.event :as event]
            [clojure.test :refer :all]
            [clj-time.instant]
            [clj-time.core :as t]
            [clj-time.coerce :as c]
            [reactor.handlers.helpers :as helpers]
            [reactor.handlers.operations :as sut]
            [toolbelt.datomic :as td]
            [toolbelt.datomic.test :as tdt]
            [fixturex.core :refer [with-fixtures]]
            [datomic.api :as d]
            [toolbelt.async :as ta]
            [toolbelt.date :as date]))


;; ==============================================================================
;; global fixtures ==============================================================
;; ==============================================================================


(use-fixtures :each (tdt/conn-fixture #(blueprints/conform-db % :db.part/starcity)))


;; ==============================================================================
;; constants ====================================================================
;; ==============================================================================


(def ^:private run-time
  "The time at which the handlers within these tests will be run."
  #inst "2018-01-01")


(def ^:private run-time-dt
  (c/to-date-time run-time))


;; ==============================================================================
;; tests ========================================================================
;; ==============================================================================


;; helpers ======================================================================


(defn- transaction-fixture [tx-data]
  (fn [test-fn]
    (tdt/with-conn conn
      @(d/transact conn tx-data)
      (test-fn))))


;; daily operations =============================================================


(deftest daily-operations
  (tdt/with-conn conn
    (let [[ev tx] (helpers/dispatch conn :ops/daily :params {:t run-time})]

      (is (= 6 (count tx))
          "a transaction of the expected shape is produced")

      (is (every? (comp (partial = (td/id ev)) :event/triggered-by) tx)
          "all events are triggered by this one")

      (is (every? (comp (partial = run-time) :t event/params) tx)
          "the `t` parameter is set to the same one as the triggering event")

      (is (= #{:reactor.handlers.operations/report-untransitioned-licenses
               :reactor.handlers.operations/send-renewal-reminders
               :reactor.handlers.operations/create-month-to-month-renewals
               :reactor.handlers.operations/deactivate-expired-licenses
               :reactor.handlers.operations/activate-pending-licenses
               :reactor.handlers.operations/cancel-transitioning-orders}
             (set (map event/key tx)))
          "all expected events are triggered"))))


;; renewal reminders ============================================================


(def ^:private licenses-tx-data
  [{:db/id                 (d/tempid :db.part/starcity)
    :member-license/ends   (c/to-date (t/plus run-time-dt (t/days 45)))
    :member-license/unit   [:unit/name "52gilbert-1"]
    :member-license/status :member-license.status/active}
   {:db/id                 (d/tempid :db.part/starcity)
    :member-license/ends   (c/to-date (t/plus run-time-dt (t/days 45)))
    :member-license/unit   [:unit/name "52gilbert-2"]
    :member-license/status :member-license.status/inactive}
   {:db/id                 (d/tempid :db.part/starcity)
    :member-license/ends   (c/to-date (t/plus run-time-dt (t/days 30)))
    :member-license/unit   [:unit/name "52gilbert-2"]
    :member-license/status :member-license.status/active}])


(deftest send-renewal-reminders
  (tdt/with-conn conn
    (with-fixtures [(transaction-fixture licenses-tx-data)]
      (let [key      :reactor.handlers.operations/send-renewal-reminders
            [ev res] (helpers/dispatch conn key :params {:t        run-time
                                                         :interval sut/reminder-interval})]

        (testing "transaction shape"
          (is (and (= 2 (count res)) (every? map? res))
              "there are two maps to transact"))

        (testing "transaction contents"
          (let [[reminders-ev reminder-ev] res]

            (testing "additional reminders event"
              (is (= key (event/key reminders-ev)))
              (is (= :job (event/topic reminders-ev)))
              (is (= (rest sut/reminder-interval) (:interval (event/params reminders-ev))))
              (is (= (td/id ev) (event/triggered-by reminders-ev)))
              (is (= run-time (:t (event/params reminders-ev)))))

            (testing "individual reminder event"
              (is (= :reactor.handlers.operations/send-renewal-reminder (event/key reminder-ev)))
              (is (= :notify (event/topic reminder-ev)))
              (is (= 45 (:days (event/params reminder-ev))))
              (is (= (td/id ev) (event/triggered-by reminder-ev)))

              (let [license (d/entity (d/db conn) (:license-id (event/params reminder-ev)))]
                (is (some? license))
                (is (= :member-license.status/active (:member-license/status license)))))))))))
