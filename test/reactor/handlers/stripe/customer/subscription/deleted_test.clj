(ns reactor.handlers.stripe.customer.subscription.deleted-test
  (:require [clojure.core.async :as a]
            [clojure.test :refer :all]
            [mock.mock :as mock]
            mock.stripe.event
            [reactor.fixtures :as fixtures :refer [with-conn]]
            [reactor.handlers.stripe.test-utils :as tu]
            [reactor.handlers.stripe.customer.subscription.deleted]
            [ribbon.event :as re]
            [toolbelt.datomic :as td]
            [toolbelt.predicates :as p]
            [toolbelt.core :as tb]
            [reactor.models.event :as event]
            [blueprints.models.account :as account]
            [reactor.handlers.helpers :as helpers]))


(use-fixtures :once fixtures/conn-fixture)


(deftest subscription-deleted
  (let [stripe-event mock.stripe.event/subscription-deleted
        scenario     (partial tu/speculate-scenario
                        :stripe.event.customer.subscription/deleted
                        stripe-event)]

    (with-conn conn
      (testing "subscriptions not associated with a rent payment or service order produce no transaction"
        (let [{tx :tx} (scenario conn)]
          (is (nil? tx))))

      (testing "subscriptions associated with rent payments"
        (let [license (mock/member-license-tx :sub-id (re/subject-id stripe-event))
              account (mock/account-tx :license (td/id license))
              {tx :tx :as out} (scenario conn account license)]

          (testing "transaction shape"
            (is (vector? tx))
            (is (= 3 (count tx))))

          (testing "will retract subscription"
            (let [[op eid attr v] (tb/find-by vector? tx)]
              (is (= op :db/retract))
              (is (integer? eid))
              (is (= attr :member-license/subscription-id))
              (is (= v (re/subject-id stripe-event)))))

          (testing "will notify member"
            (let [ev (tb/find-by event/notify? tx)]
              (is (= (event/key ev) :stripe.event.customer.subscription.deleted/rent))
              (is (integer? (-> ev event/params :account-id)))
              (is (= (td/id (:event out)) (-> ev event/triggered-by td/id))))))))))


(deftest notify-event
  (let [account (mock/account-tx)
        event   (event/notify :stripe.event.customer.subscription.deleted/rent
                              {:params {:account-id [:account/email (account/email account)]}})]
    (with-conn conn
      (let [{tx :tx} (helpers/dispatch-event conn event account)]
        (testing "produces channel"
          (is (p/chan? tx))
          (is (map? (a/<!! tx))))))))


(deftest report-event
  (let [account (mock/account-tx)
        event   (event/report :stripe.event.customer.subscription.deleted/rent
                              {:params {:account-id [:account/email (account/email account)]}})]
    (with-conn conn
      (let [{tx :tx} (helpers/dispatch-event conn event account)]
        (testing "produces channel"
          (is (p/chan? tx))
          (is (map? (a/<!! tx))))))))
