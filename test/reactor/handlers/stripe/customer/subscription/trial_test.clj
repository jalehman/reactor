(ns reactor.handlers.stripe.customer.subscription.trial-test
  (:require [clojure.core.async :as a]
            [clojure.test :refer :all]
            [mock.mock :as mock]
            mock.stripe.event
            [reactor.fixtures :as fixtures :refer [with-conn]]
            [reactor.handlers.stripe.test-utils :as tu]
            [reactor.handlers.stripe.customer.subscription.trial]
            [ribbon.event :as re]
            [toolbelt.async :as ta]
            [toolbelt.datomic :as td]))

(use-fixtures :once fixtures/conn-fixture)


(deftest trial-will-end
  (let [stripe-event mock.stripe.event/trial-will-end
        scenario     (partial tu/speculate-scenario
                        :stripe.event.customer.subscription/trial-will-end
                        stripe-event)]

    (with-conn conn
      (testing "subscriptions not associated with a rent payment produce no transaction"
        (let [{tx :tx} (scenario conn)]
          (is (nil? tx))))

      (testing "subscriptions associated with a rent payment send an email"
        (let [license (mock/member-license-tx :sub-id (re/subject-id stripe-event))
              account (mock/account-tx :license (td/id license))
              {tx :tx :as out} (scenario conn account license)]

          (is (ta/chan? tx))
          (is (map? (a/<!! tx))))))))
