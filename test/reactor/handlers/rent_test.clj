(ns reactor.handlers.rent-test
  (:require [blueprints.models.event :as event]
            [blueprints.models.member-license :as member-license]
            [blueprints.models.payment :as payment]
            [clj-time.coerce :as c]
            [clj-time.core :as t]
            [clojure.core.async :as a]
            [clojure.test :refer :all]
            [datomic.api :as d]
            [reactor.fixtures :as fixtures :refer [with-conn]]
            [reactor.handlers.helpers :as helpers]
            [reactor.handlers.rent :as rent]
            [toolbelt.core :as tb]
            [toolbelt.async :as ta]))

;; (def test-licenses
;;   [;; valid license
;;    {:db/id                       (d/tempid :db.part/starcity -1)
;;     :member-license/status       :member-license.status/active
;;     :member-license/unit         [:unit/name "52gilbert-1"]
;;     :member-license/price        2100.0
;;     :member-license/commencement (c/to-date (t/date-time 2017 1 1))}
;;    ;; uncommenced
;;    {:db/id                       (d/tempid :db.part/starcity -2)
;;     :member-license/status       :member-license.status/active
;;     :member-license/unit         [:unit/name "52gilbert-2"]
;;     :member-license/price        2100.0
;;     :member-license/commencement (c/to-date (t/date-time 2017 1 10))}
;;    ;; inactive
;;    {:db/id                       (d/tempid :db.part/starcity -3)
;;     :member-license/status       :member-license.status/inactive
;;     :member-license/unit         [:unit/name "52gilbert-3"]
;;     :member-license/price        2100.0
;;     :member-license/commencement (c/to-date (t/date-time 2017 1 1))}
;;    ;; autopay
;;    {:db/id                          (d/tempid :db.part/starcity -4)
;;     :member-license/status          :member-license.status/active
;;     :member-license/unit            [:unit/name "52gilbert-4"]
;;     :member-license/price           2100.0
;;     :member-license/commencement    (c/to-date (t/date-time 2017 1 1))
;;     :member-license/subscription-id "abcd"}])


;; (def test-accounts
;;   [{:db/id              (d/tempid :db.part/starcity)
;;     :account/first-name "Jocelyn"
;;     :account/last-name  "Robancho"
;;     :account/email      "test1@test.com"
;;     :account/licenses   (d/tempid :db.part/starcity -1)}
;;    {:db/id              (d/tempid :db.part/starcity)
;;     :account/first-name "Jocelyn"
;;     :account/last-name  "Robancho"
;;     :account/email      "test2@test.com"
;;     :account/licenses   (d/tempid :db.part/starcity -2)}
;;    {:db/id              (d/tempid :db.part/starcity)
;;     :account/first-name "Jocelyn"
;;     :account/last-name  "Robancho"
;;     :account/email      "test3@test.com"
;;     :account/licenses   (d/tempid :db.part/starcity -3)}
;;    {:db/id              (d/tempid :db.part/starcity)
;;     :account/first-name "Jocelyn"
;;     :account/last-name  "Robancho"
;;     :account/email      "test4@test.com"
;;     :account/licenses   (d/tempid :db.part/starcity -4)}])


;; (defn setup-active-licenses [conn]
;;   @(d/transact conn (concat test-accounts test-licenses)))


;; (deftest active-licenses
;;   (with-conn conn
;;     (setup-active-licenses conn)
;;     (testing "queried licenses are active, have commenced, and are not on autopay"
;;       (let [period (t/date-time 2017 1 1 1)
;;             qres   (rent/active-licenses (d/db conn) (c/to-date period))]
;;         (is (= 1 (count qres))
;;             "of the licenses in `test-licenses`, only the first one should be produced by the query.")

;;         (let [license (d/entity (d/db conn) (ffirst qres))]
;;           (is (t/before? (c/to-date-time (member-license/commencement license)) period)
;;               "license has commenced")
;;           (is (nil? (member-license/subscription-id license))
;;               "not on autopay")
;;           (is (= :member-license.status/active (member-license/status license))
;;               "license is active"))))))


;; (deftest create-rent-payments
;;   (with-conn conn
;;     (setup-active-licenses conn)
;;     (let [params  {:period (c/to-date (t/date-time 2017 1 15))}
;;           [ev tx] (helpers/dispatch conn :rent-payments/create-all :params params)]

;;       (testing "`:rent-payments/create-all` produces valid `:rent-payment/create` events for each active license"
;;         (is (= 2 (count tx)))
;;         (is (every? #(= :rent-payment/create (:event/key %)) tx)))

;;       (testing "must supply time period"
;;         (is (thrown? java.lang.AssertionError
;;                      (helpers/dispatch conn :rent-payments/create-all)))))))


;; (deftest create-rent-payment
;;   (with-conn conn
;;     (setup-active-licenses conn)
;;     (let [account (d/entity (d/db conn) [:account/email "test1@test.com"])
;;           start   (c/to-date (t/date-time 2017 1 1))
;;           end     (c/to-date (t/date-time 2017 1 31))
;;           amount  2100.0
;;           params  {:member-license-id (:db/id (member-license/active (d/db conn) account))
;;                    :amount            amount
;;                    :start             start
;;                    :end               end}
;;           [ev tx] (helpers/dispatch conn :rent-payment/create :params params)]

;;       (testing "produces a transaction with two entities"
;;         (is (vector? tx))
;;         (is (= 2 (count tx))))

;;       (testing "produces a transaction with a valid rent payment"
;;         (let [py (:member-license/rent-payments (tb/find-by :member-license/rent-payments tx))]
;;           (is (map? py))
;;           (is (= (payment/status py) :payment.status/due))
;;           (is (= start (payment/period-start py)))
;;           (is (= end (payment/period-end py)))
;;           (is (= 2100.0 (payment/amount py)))))

;;       (testing "produces a transaction with a notify event"
;;         (let [ev' (tb/find-by :event/key tx)]
;;           (is (map? ev') "there's an event")
;;           (is (event/notify? ev'))
;;           (is (= :rent-payment/create (:event/key ev')))
;;           (is (= (:db/id ev) (:event/triggered-by ev'))))))))


;; (deftest send-rent-email-reminder
;;   (with-conn conn
;;     (setup-active-licenses conn)
;;     (let [account  (d/entity (d/db conn) [:account/email "test1@test.com"])
;;           license  (member-license/active (d/db conn) account)
;;           event    (event/notify :rent-payment/create {:params {:amount            2100.0
;;                                                                 :member-license-id (:db/id license)}})
;;           {tx :tx} (helpers/dispatch-event conn event)]

;;       (testing "a channel is produced"
;;         (is (ta/chan? tx))
;;         (is (map? (a/<!! tx)))))))
