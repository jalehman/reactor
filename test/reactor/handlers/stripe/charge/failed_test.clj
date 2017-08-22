(ns reactor.handlers.stripe.charge.failed-test
  (:require [blueprints.models.account :as account]
            [blueprints.models.event :as event]
            [blueprints.models.member-license :as member-license]
            [blueprints.models.payment :as payment]
            [blueprints.models.security-deposit :as deposit]
            [clojure.core.async :as a]
            [clojure.test :refer :all]
            [datomic.api :as d]
            [mock.mock :as mock]
            mock.stripe.event
            [reactor.dispatch :as dispatch]
            [reactor.fixtures :as fixtures :refer [with-conn]]
            [reactor.handlers.helpers :refer :all]
            reactor.handlers.stripe.charge.failed
            [reactor.handlers.stripe.test-utils :as tu]
            [toolbelt.core :as tb]
            [toolbelt.datomic :as td]
            [toolbelt.predicates :as p]))

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


(deftest can-handle-failed-payments
  (with-conn conn
    (testing "payments that have already failed cannot be processed more than once"
      (let [account (mock/account-tx)
            payment (payment/create 10.0 account
                                    :charge-id mock-subj
                                    :status :payment.status/failed)]
        (is (thrown-with-msg? java.lang.AssertionError #"Payment has already failed"
                              (scenario conn account payment)))))


    (testing "failed payments not associated with a rent payment, service order or deposit"
      (let [account  (mock/account-tx)
            payment  (payment/create 10.0 account
                                     :charge-id mock-subj)
            {tx :tx} (scenario conn account payment)]

        (testing "transaction validity"
          (is (sequential? tx))
          (is (= 1 (count tx))))

        (testing "only marked as failed"
          (is (= :payment.status/failed (-> tx first :payment/status))))))


    (testing "failed security deposit charges"
      (let [account            (mock/account-tx)
            deposit            (deposit/create account 2100.0)
            payment            (payment/create 2100.0 account
                                               :for :payment.for/deposit
                                               :charge-id mock-subj)
            {:keys [event tx]} (scenario conn account payment (deposit/add-payment deposit payment))]

        (testing "transition validity"
          (is (sequential? tx))
          (is (= 3 (count tx))))

        (testing "produces internal notification event"
          (let [ev (tb/find-by event/report? tx)]
            (is (= (td/id event) (-> ev event/triggered-by td/id)))
            (is (= :reactor.handlers.stripe.charge.failed/notify.deposit (event/key ev)))

            (let [{:keys [account-id payment-id]} (event/params ev)]
              (is (integer? account-id))
              (is (integer? payment-id)))))

        (testing "produces event to notify member"
          (let [ev (tb/find-by event/notify? tx)]
            (is (= (td/id event) (-> ev event/triggered-by td/id)))
            (is (= :reactor.handlers.stripe.charge.failed/notify.deposit (event/key ev)))

            (let [{:keys [account-id payment-id]} (event/params ev)]
              (is (integer? account-id))
              (is (integer? payment-id)))))))


    (testing "failed rent charge"
      (let [license            (mock/member-license-tx :ref -1)
            account            (mock/account-tx :license (td/id license))
            paid-on            (java.util.Date.)
            payment            (payment/create 2100.0 account
                                               :pstart (java.util.Date.)
                                               :pend (java.util.Date.)
                                               :status :payment.status/due
                                               :charge-id mock-subj
                                               :paid-on paid-on
                                               :due (java.util.Date.)
                                               :method :payment.method/stripe-charge)
            {:keys [event tx]} (scenario conn license account
                                         (member-license/add-rent-payments license payment))]

        (testing "tranasction validity"
          (is (sequential? tx))
          (is (= 4 (count tx))))

        (testing "will retrace the `:payment/paid-on` timestamp"
          (let [[op e a v] (tb/find-by vector? tx)]
            (is (= op :db/retract))
            (is (integer? e))
            (is (= a :payment/paid-on))
            (is (= v paid-on))))

        (testing "the payment is marked as failed"
          (let [ch (tb/find-by :payment/status tx)]
            (is (= :payment.status/failed (:payment/status ch)))))

        (testing "produces internal notification event"
          (let [ev' (tb/find-by event/report? tx)]
            (is (= (td/id event) (-> ev' event/triggered-by td/id)))

            (let [{:keys [account-id payment-id]} (event/params ev')]
              (is (integer? account-id))
              (is (integer? payment-id)))))))))


(deftest notify-internal
  (with-conn conn
    (let [account (mock/account-tx)
          payment (payment/create 2100.0 account
                                  :charge-id mock-subj)
          deposit (deposit/create account 2100)
          event   (event/report :reactor.handlers.stripe.charge.failed/notify.deposit
                                {:params {:account-id [:account/email (account/email account)]
                                          :payment-id [:stripe/charge-id mock-subj]}})
          db      (:db-after (d/with (d/db conn) [account payment event deposit (deposit/add-payment deposit payment)]))
          event   (event/by-uuid db (event/uuid event))
          c       (dispatch/report (deps db) event (event/params event))]

      (testing "produces a channel"
        (is (p/chan? c))
        (is (map? (a/<!! c)))))))
