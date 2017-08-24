(ns reactor.handlers.stripe.charge.pending-test
  (:require [blueprints.models.payment :as payment]
            [clojure.test :refer :all]
            [mock.mock :as mock]
            mock.stripe.event
            [reactor.fixtures :as fixtures :refer [with-conn]]
            [reactor.handlers.stripe.test-utils :as tu]
            [ribbon.event :as re]))

(use-fixtures :once fixtures/conn-fixture)


(deftest charge-pending
  (let [stripe-event mock.stripe.event/pending-charge
        scenario     (partial tu/speculate-scenario
                        :stripe.event.charge/pending
                        stripe-event)]
    (with-conn conn
      (testing "pending charges events result in the source being added to the payment"
        (let [account  (mock/account-tx)
              payment  (payment/create 10.0 account
                                       :charge-id (re/subject-id stripe-event))
              {tx :tx} (scenario conn account payment)]

          (testing "transaction validity"
            (is (map? tx)))

          (testing "source is added"
            (is (= (payment/source-id tx) (get-in (re/subject stripe-event) [:source :id]))))))

      (testing "pending charge events without a backing payment result in no tx"
        (let [{tx :tx} (scenario conn)]
          (is (nil? tx)))))))
