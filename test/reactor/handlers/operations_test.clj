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
            [toolbelt.date :as date]
            [blueprints.models.property :as property]
            [blueprints.models.member-license :as member-license]
            [blueprints.models.license :as license]
            [toolbelt.core :as tb]
            [blueprints.models.license-transition :as transition]))


(def ^:private part
  :db.part/starcity)


;; ==============================================================================
;; global fixtures ==============================================================
;; ==============================================================================


(use-fixtures :each (tdt/conn-fixture #(blueprints/conform-db % part)))


;; ==============================================================================
;; constants ====================================================================
;; ==============================================================================


(def ^:private tz
  (t/time-zone-for-id "America/Los_Angeles"))


(def ^:private run-time
  "The time at which the handlers within these tests will be run."
  (date/beginning-of-day #inst "2018-01-01" tz))


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


(defn- mock-license
  [unit & {:keys [ends ends-days-from-now status term]
           :or   {status :member-license.status/active
                  term   3}}]
  (tdt/with-conn conn
    (tb/assoc-when
     (member-license/create (license/by-term (d/db conn) term)
                            unit
                            #inst "2018-01-01"
                            2000.0
                            status)
     :member-license/ends (when (or (some? ends) (some? ends-days-from-now))
                            (-> (or ends (c/to-date (t/plus run-time-dt (t/days ends-days-from-now))))
                                (date/end-of-day tz))))))


(defn- mock-transition
  [curr-license-id]
  {:db/id                              (d/tempid part)
   :license-transition/current-license curr-license-id})


(defn- license-by-unit
  "Assumes that there's only one license with this unit."
  [db unit]
  (->> (d/q '[:find ?l .
              :in $ ?u
              :where
              [?l :member-license/unit ?u]]
            db (td/id unit))
       (d/entity db)))


;; daily operations =============================================================


(deftest daily-operations
  (tdt/with-conn conn
    (let [[ev tx] (helpers/dispatch conn :ops/daily :params {:t run-time})]

      (is (= 4 (count tx))
          "a transaction of the expected shape is produced")

      (is (every? (comp (partial = (td/id ev)) :event/triggered-by) tx)
          "all events are triggered by this one")

      (is (every? (comp (partial = run-time) :t event/params) tx)
          "the `t` parameter is set to the same one as the triggering event")

      (is (= #{:reactor.handlers.operations/report-untransitioned-licenses
               :reactor.handlers.operations/send-renewal-reminders
               :reactor.handlers.operations/create-month-to-month-renewals
               :reactor.handlers.operations/cancel-transitioning-orders}
             (set (map event/key tx)))
          "all expected events are triggered"))))


;; renewal reminders ============================================================


(deftest send-renewal-reminders
  (tdt/with-conn conn
    (with-fixtures [(transaction-fixture
                     [(mock-license [:unit/name "52gilbert-1"] :ends-days-from-now 45)
                      (mock-license [:unit/name "52gilbert-2"] :ends-days-from-now 45
                                    :status :member-license.status/inactive)
                      (mock-license [:unit/name "52gilbert-3"] :ends-days-from-now 30)])]
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


;; create month to month renewals ===============================================


(defn- next-day [date]
  (let [date (date/tz-uncorrected date tz)]
    (c/to-date (t/plus (c/to-date-time date) (t/days 1)))))


(defn- month-after [date]
  (-> (date/tz-uncorrected date tz)
      (c/to-date-time)
      (t/plus (t/months 1))
      (t/minus (t/days 1))
      (c/to-date)))


(deftest create-month-to-month-renewal
  (tdt/with-conn conn
    (with-fixtures [(transaction-fixture
                     [(mock-license [:unit/name "52gilbert-1"] :ends-days-from-now 30)])]
      (let [key      :reactor.handlers.operations/create-month-to-month-transition
            license  (license-by-unit (d/db conn) [:unit/name "52gilbert-1"])
            [ev res] (helpers/dispatch conn key :params {:license-id (td/id license)})

            [new-license transition event] res]

        (testing "validity of new license"
          (is (= :member-license.status/pending (:member-license/status new-license))
              "a pending license is created")
          (is (= (:member-license/rate license) (:member-license/rate new-license))
              "the old and new license have the same rate")

          (testing "start and end dates"
            (is (= (member-license/starts new-license)
                   (date/beginning-of-day (next-day (member-license/ends license)) tz))
                "the new license starts at the beginning of the day the existing license end")

            (is (= (member-license/ends new-license)
                   (date/end-of-day (month-after (member-license/starts new-license)) tz))
                "the new license ends at the end of the day one month (less a
                day) after the start date")))

        (testing "validity of license transition"
          (is (= (transition/current-license transition) (td/id license)))
          (is (= (transition/date transition) (member-license/starts new-license)))
          (is (= (transition/type transition) :license-transition.type/renewal)))

        (is (= (event/key event) :transition/month-to-month-created))))))


(deftest month-to-month-renewals
  (tdt/with-conn conn
    (with-fixtures [(transaction-fixture
                     (let [transition-license (mock-license [:unit/name "52gilbert-4"] :ends-days-from-now 30)]
                       [(mock-license [:unit/name "52gilbert-1"] :ends-days-from-now 31) ; because January has 31 days
                        (mock-license [:unit/name "52gilbert-2"] :ends-days-from-now 30)
                        (mock-license [:unit/name "52gilbert-3"] :ends-days-from-now 29)
                        transition-license
                        (mock-transition (:db/id transition-license))]))]
      (let [key      :reactor.handlers.operations/create-month-to-month-renewals
            [ev res] (helpers/dispatch conn key :params {:t run-time})]

        (is (= 3 (count res)) "the transaction has the correct shape")

        (is (= :reactor.handlers.operations/activate-pending-licenses
               (event/key (first res))))

        (is (= :reactor.handlers.operations/deactivate-expired-licenses
               (event/key (second res))))

        (testing "only licenses without transitions that end in 30 days are renewed"
          (let [license (d/entity (d/db conn) (:license-id (event/params (last res))))]
            (is (= "52gilbert-1" (-> license :member-license/unit :unit/name))
                "the license in the event is the one that ends in 30 days")))))))
