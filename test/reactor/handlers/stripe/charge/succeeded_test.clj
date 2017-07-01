(ns reactor.handlers.stripe.charge.succeeded-test
  (:require [blueprints.models
             [charge :as charge]
             [order :as order]
             [payment :as payment]
             [rent-payment :as rent-payment]
             [security-deposit :as deposit]
             [service :as service]]
            [clojure.test :refer :all]
            [datomic.api :as d]
            [mock.mock :as mock]
            [mock.stripe.event]
            [reactor.fixtures :as fixtures :refer [with-conn]]
            [reactor.handlers.stripe.test-utils :as tu]
            [reactor.handlers.stripe.charge.succeeded]
            [toolbelt
             [core :as tb]
             [datomic :as td]]))


(use-fixtures :once fixtures/conn-fixture)


;; =============================================================================
;; Helpers
;; =============================================================================


(def ^:private event-key :stripe.event.charge/succeeded)

(def ^:private mock-event mock.stripe.event/successful-charge)

(def mock-subj (tu/mock-subj mock-event))

(def mock-id (tu/mock-id mock-event))

(def mock-amount (tu/mock-amount mock-event))

(def ^:private scenario
  (partial tu/speculate-scenario event-key mock-event))


;; =============================================================================
;; Tests
;; =============================================================================


(deftest can-handle-successful-charges
  (with-conn conn
    #_(testing "charges must have a backing charge entity"
        (is (thrown? java.lang.Exception (scenario-charge conn :event-key))
            "or the event will fail"))

    (testing "charges that have already succeeded cannot be processed more than once"
      (let [account (mock/account-tx)
            charge  (charge/create account mock-subj 10.0
                                   :status :charge.status/succeeded)]
        (is (thrown-with-msg? java.lang.AssertionError #"Charge has already succeeded"
                              (scenario conn account charge)))))

    (testing "successful charges not associated with a rent payment, service order or deposit"
      (let [account  (mock/account-tx)
            charge   (charge/create account mock-subj 10.0)
            {tx :tx} (scenario conn account charge)]

        (testing "transaction validity"
          (is (sequential? tx))
          (is (= 1 (count tx))))

        (testing "only marked as succeeded"
          (is (= :charge.status/succeeded (-> tx first :charge/status))))))


    (testing "successful security deposit charges"
      (let [account  (mock/account-tx)
            deposit  (deposit/create account 2100)
            charge   (charge/create account mock-subj 10.0)
            {tx :tx} (scenario conn account (deposit/add-charge deposit charge) charge)]

        (testing "transaction validity"
          (is (sequential? tx))
          (is (= 2 (count tx))))

        (testing "contains a security deposit with updated amount received"
          (let [x (tb/find-by :security-deposit/amount-received tx)]
            (is (< (deposit/amount-received deposit) (deposit/amount-received x))
                "the amount received before the event is less than after the event")
            (is (= (int (charge/amount charge)) (- (deposit/amount-received x)
                                                   (deposit/amount-received deposit)))
                "the difference of the before and after deposit amount is equal to the charge amount")))

        (testing "the charge is marked as succeeded"
          (let [ch (tb/find-by :charge/status tx)]
            (is (= :charge.status/succeeded (:charge/status ch)))))))


    (testing "successful rent payment charges"
      (let [license  (mock/member-license-tx :ref -1)
            account  (mock/account-tx :license (td/id license))
            py       (rent-payment/create 2100.0 (java.util.Date.) (java.util.Date.)
                                          :rent-payment.status/due
                                          :due-date (java.util.Date.)
                                          :method :rent-payment.method/ach)
            charge   (charge/create account mock-subj 2100.0)
            {tx :tx} (scenario conn license account charge (assoc py :rent-payment/charge (td/id charge)))]

        (testing "tranasction validity"
          (is (sequential? tx))
          (is (= 2 (count tx))))

        (testing "contains a rent payment that is considered paid"
          (let [py (tb/find-by :rent-payment/status tx)]
            (is (= :rent-payment.status/paid (rent-payment/status py)))))

        (testing "the charge is marked as succeeded"
          (let [ch (tb/find-by :charge/status tx)]
            (is (= :charge.status/succeeded (:charge/status ch)))))))


    (testing "successful service charges"
      (let [account  (mock/account-tx)
            service  (service/moving-assistance (d/db conn))
            order    (order/create account service {:quantity 1.0})
            charge   (charge/create account mock-subj 100.0) ; NOTE: remove after transition to unified payments
            payment  (payment/create 100.0)
            {tx :tx} (scenario conn account charge payment (payment/add-charge payment mock-subj) (order/add-payment order payment))]

        (testing "tranasction validity"
          (is (sequential? tx))
          (is (= 2 (count tx))))

        (testing "the charge is marked as succeeded"
          (let [ch (tb/find-by :charge/status tx)]
            (is (charge/succeeded? ch))))

        (testing "the payment is marked as paid"
          (let [py (tb/find-by :payment/status tx)]
            (is (payment/paid? py))))))))
