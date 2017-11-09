(ns reactor.handlers.stripe.charge.succeeded-test
  (:require [blueprints.models.member-license :as member-license]
            [blueprints.models.order :as order]
            [blueprints.models.payment :as payment]
            [blueprints.models.security-deposit :as deposit]
            [blueprints.models.service :as service]
            [clojure.test :refer :all]
            [datomic.api :as d]
            [mock.mock :as mock]
            mock.stripe.event
            [reactor.fixtures :as fixtures :refer [with-conn]]
            [reactor.handlers.stripe.test-utils :as tu]
            [toolbelt.core :as tb]
            [toolbelt.datomic :as td]))

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

    (testing "payment that have already succeeded cannot be processed more than once"
      (let [account (mock/account-tx)
            payment (payment/create 10.0 account
                                    :charge-id mock-subj
                                    :status :payment.status/paid)]
        (is (thrown-with-msg? java.lang.AssertionError #"Payment has already succeeded"
                              (scenario conn account payment)))))

    (testing "successful payments not associated with a rent payment, service order or deposit"
      (let [account  (mock/account-tx)
            payment  (payment/create 10.0 account
                                     :charge-id mock-subj)
            {tx :tx} (scenario conn account payment)]

        (testing "transaction validity"
          (is (sequential? tx))
          (is (= 1 (count tx))))

        (testing "only marked as succeeded"
          (is (= :payment.status/paid (-> tx first :payment/status))))))


    (testing "successful security deposit charges"
      (let [account  (mock/account-tx)
            deposit  (deposit/create account 2100.0)
            payment  (payment/create 2100.0 account
                                     :for :payment.for/deposit
                                     :charge-id mock-subj)
            {tx :tx} (scenario conn account payment
                               (deposit/add-payment deposit payment))]

        (testing "transaction validity"
          (is (sequential? tx))
          (is (= 1 (count tx))))

        #_(testing "contains a security deposit with updated amount received"
            (let [x (tb/find-by :security-deposit/amount-received tx)]
              (is (< (deposit/amount-received deposit) (deposit/amount-received x))
                  "the amount received before the event is less than after the event")
              (is (= (int (charge/amount charge)) (- (deposit/amount-received x)
                                                     (deposit/amount-received deposit)))
                  "the difference of the before and after deposit amount is equal to the charge amount")))

        (testing "the charge is marked as succeeded"
          (let [ch (tb/find-by :payment/status tx)]
            (is (= :payment.status/paid (:payment/status ch)))))))


    (testing "successful rent payments"
      (let [license  (mock/member-license-tx :ref -1)
            account  (mock/account-tx :license (td/id license))
            payment  (payment/create 2100.0 account
                                     :charge-id mock-subj
                                     :pstart (java.util.Date.)
                                     :pend (java.util.Date.)
                                     :status :payment.status/due
                                     :due (java.util.Date.)
                                     :method :payment.method/stripe-charge)
            {tx :tx} (scenario conn license account
                               (member-license/add-rent-payments license payment))]

        (testing "tranasction validity"
          (is (sequential? tx))
          (is (= 1 (count tx))))

        (testing "contains a rent payment that is considered paid"
          (let [py (tb/find-by :payment/status tx)]
            (is (payment/paid? py))))))


    (testing "successful service charges"
      (let [account  (mock/account-tx)
            service  (service/moving-assistance (d/db conn))
            order    (order/create account service {:quantity 1.0
                                                    :status   :order.status/placed})
            payment  (payment/create 100.0 account
                                     :for :payment.for/order
                                     :charge-id mock-subj)
            {tx :tx} (scenario conn account payment order
                               (order/add-payment order payment))]

        (testing "tranasction validity"
          (is (sequential? tx))
          (is (= 2 (count tx))))

        (testing "the payment is marked as paid"
          (let [py (tb/find-by :payment/status tx)]
            (is (payment/paid? py))))

        (testing "the payment will ahve a `:payment/paid-on` date added"
          (let [[_ _ attr v] (tb/find-by vector? tx)]
            (is (= attr :payment/paid-on))
            (is (inst? v))))))))
