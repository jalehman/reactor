(ns reactor.handlers.stripe.charge.failed-test
  (:require [blueprints.models
             [account :as account]
             [charge :as charge]
             [rent-payment :as rent-payment]
             [security-deposit :as deposit]]
            [clojure.core.async :as a]
            [clojure.test :refer :all]
            [datomic.api :as d]
            [mock.mock :as mock]
            mock.stripe.event
            [reactor
             [dispatch :as dispatch]
             [fixtures :as fixtures :refer [with-conn]]]
            [reactor.handlers.helpers :refer :all]
            [reactor.handlers.stripe.test-utils :as tu]
            [reactor.handlers.stripe.charge.failed]
            [reactor.models.event :as event]
            [toolbelt
             [core :as tb]
             [datomic :as td]
             [predicates :as p]]))

(use-fixtures :once fixtures/conn-fixture)


;; =============================================================================
;; Helpers
;; =============================================================================


(def ^:private event-key :stripe.event.charge/failed)

(def ^:private mock-event mock.stripe.event/failed-charge)

(def mock-subj (tu/mock-subj mock-event))

(def mock-id (tu/mock-id mock-event))

(def mock-amount (tu/mock-amount mock-event))


(def ^:private scenario
  (partial tu/speculate-scenario event-key mock-event))


;; =============================================================================
;; Tests
;; =============================================================================


(deftest can-handle-failed-charges
  (with-conn conn
    (testing "charges that have already failed cannot be processed more than once"
      (let [account (mock/account-tx)
            charge  (charge/create account mock-subj 10.0
                                   :status :charge.status/failed)]
        (is (thrown-with-msg? java.lang.AssertionError #"Charge has already failed"
                              (scenario conn account charge)))))


    (testing "failed charges not associated with a rent payment, service order or deposit"
      (let [account  (mock/account-tx)
            charge   (charge/create account mock-subj 10.0)
            {tx :tx} (scenario conn account charge)]

        (testing "transaction validity"
          (is (sequential? tx))
          (is (= 1 (count tx))))

        (testing "only marked as failed"
          (is (= :charge.status/failed (-> tx first :charge/status))))))


    (testing "failed security deposit charges"
      (let [account            (mock/account-tx)
            charge             (charge/create account mock-subj 500.0)
            deposit            (deposit/create account 2100)
            {:keys [event tx]} (scenario conn account charge (deposit/add-charge deposit charge))]

        (testing "transition validity"
          (is (sequential? tx))
          (is (= 3 (count tx))))

        (testing "produces internal notification event"
          (let [ev' (tb/find-by :event/key tx)]
            (is (= :slack (event/topic ev')))
            (is (= (td/id event) (-> ev' event/triggered-by td/id)))

            (let [{:keys [account-id charge-id type]} (event/params ev')]
              (is (= type :security-deposit))
              (is (integer? account-id))
              (is (integer? charge-id)))))))


    (testing "failed rent charge"
      (let [license            (mock/member-license-tx :ref -1)
            account            (mock/account-tx :license (td/id license))
            paid-on            (java.util.Date.)
            payment            (rent-payment/create 2100.0 (java.util.Date.) (java.util.Date.)
                                                    :rent-payment.status/due
                                                    :paid-on paid-on
                                                    :due-date (java.util.Date.)
                                                    :method :rent-payment.method/ach)
            charge             (charge/create account mock-subj 2100.0)
            {:keys [event tx]} (scenario conn license account charge
                                         (assoc payment :rent-payment/charge (td/id charge)))]

        (testing "tranasction validity"
          (is (sequential? tx))
          (is (= 5 (count tx))))

        (testing "contains a rent payment that is due"
          (let [py (tb/find-by :rent-payment/status tx)]
            (is (= :rent-payment.status/due (rent-payment/status py)))))

        (testing "will retrace the `:rent-payment/paid-on` timestamp"
          (let [[op e a v] (tb/find-by vector? tx)]
            (is (= op :db/retract))
            (is (integer? e))
            (is (= a :rent-payment/paid-on))
            (is (= v paid-on))))

        (testing "the charge is marked as failed"
          (let [ch (tb/find-by :charge/status tx)]
            (is (= :charge.status/failed (:charge/status ch)))))

        (testing "produces internal notification event"
          (let [ev' (tb/find-by :event/key tx)]
            (is (= :slack (event/topic ev')))
            (is (= (td/id event) (-> ev' event/triggered-by td/id)))

            (let [{:keys [account-id charge-id type]} (event/params ev')]
              (is (= type :rent))
              (is (integer? account-id))
              (is (integer? charge-id)))))))))


(deftest notify-internal
  (with-conn conn
    (let [account (mock/account-tx)
          charge  (charge/create account mock-subj 10.0)
          uuid    (d/squuid)
          event   (event/create :stripe.event.charge.failed/notify-internal
                                {:uuid   uuid
                                 :params {:account-id [:account/email (account/email account)]
                                          :charge-id  [:charge/stripe-id mock-subj]
                                          :type       :security-deposit}
                                 :topic  :slack})
          db      (:db-after (d/with (d/db conn) [account charge event]))
          event   (event/by-uuid db uuid)
          c       (dispatch/slack (deps db) event (event/params event))]

      (testing "produces a channel"
        (is (p/chan? c))
        (is (map? (a/<!! c)))))))
