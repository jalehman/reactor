(ns reactor.handlers.stripe.customer.source-test
  (:require [blueprints.models
             [account :as account]
             [customer :as customer]]
            [clojure.core.async :as a]
            [clojure.test :refer :all]
            [mock.mock :as mock]
            mock.stripe.event
            [reactor
             [dispatch :as dispatch]
             [fixtures :as fixtures :refer [with-conn]]]
            [reactor.handlers.helpers :as helpers]
            [reactor.handlers.stripe.customer.source]
            [reactor.handlers.stripe.test-utils :as tu]
            [blueprints.models.event :as event]
            [ribbon.event :as re]
            [toolbelt
             [core :as tb]
             [datomic :as td]
             [predicates :as p]]))


(use-fixtures :once fixtures/conn-fixture)


;; =============================================================================
;; Tests
;; =============================================================================


(deftest verification-failed
  (let [event-key :stripe.event.customer.source/updated
        event     (mock.stripe.event/source-updated "verification_failed")
        subj      (re/subject-id event)
        id        (re/id event)
        scenario  (partial tu/speculate-scenario event-key event)]
    (with-conn conn
      (let [account            (mock/account-tx)
            customer           (customer/create (:customer (re/subject event)) account)
            {:keys [event tx]} (scenario conn account customer)]

        (testing "transaction validity"
          (is (sequential? tx))
          (is (= 3 (count tx))))

        (testing "transaction contains a report event"
          (let [ev' (tb/find-by event/report? tx)]
            (is (= (td/id event) (-> ev' event/triggered-by td/id))
                "has triggered-by set")

            (let [{:keys [account-id status]} (event/params ev')]
              (is (= "verification_failed" status))
              (is (integer? account-id)))))


        (testing "transaction contains a notify event"
          (let [ev' (tb/find-by event/notify? tx)]
            (is (= (td/id event) (-> ev' event/triggered-by td/id))
                "has triggered-by set")))

        (testing "transaction will remove the customer entity"
          (let [tx' (tb/find-by vector? tx)]
            (is (= :db.fn/retractEntity (first tx')) "will retract the entity")
            (is (= 2 (count (second tx'))) "is a lookup ref")))))))


(deftest verification-succeeded
  (let [event-key :stripe.event.customer.source/updated
        event     (mock.stripe.event/source-updated)
        subj      (re/subject-id event)
        id        (re/id event)
        scenario  (partial tu/speculate-scenario event-key event)]
    (with-conn conn
      (let [account            (mock/account-tx)
            customer           (customer/create (:customer (re/subject event)) account)
            {:keys [event tx]} (scenario conn account customer)]

        (testing "transaction validity"
          (is (sequential? tx))
          (is (= 2 (count tx))))

        (testing "transaction contains a report events"
          (let [ev' (tb/find-by event/report? tx)]
            (is (= (td/id event) (-> ev' event/triggered-by td/id))
                "has triggered-by set")

            (let [{:keys [account-id status]} (event/params ev')]
              (is (= "verified" status))
              (is (integer? account-id)))))

        (testing "transaction contains a notify event"
          (let [ev' (tb/find-by event/notify? tx)]
            (is (= (td/id event) (-> ev' event/triggered-by td/id))
                "has triggered-by set")))))))


(deftest notify-internal
  (letfn [(make-event [account status]
            (event/report :stripe.event.customer.source/updated
                          {:params {:account-id [:account/email (account/email account)]
                                    :status     status}}))]
    (with-conn conn
      (let [account  (mock/account-tx)
            {tx :tx} (helpers/dispatch-event conn (make-event account "verification_failed") account)]
        (testing "produces channel"
          (is (p/chan? tx))
          (is (map? (a/<!! tx)))))

      (let [account  (mock/account-tx)
            {tx :tx} (helpers/dispatch-event conn (make-event account "verified") account)]

        (testing "produces channel"
          (is (p/chan? tx))
          (is (map? (a/<!! tx)))))

      (let [account  (mock/account-tx)
            {tx :tx} (helpers/dispatch-event conn (make-event account "blahblahblah") account)]

        (testing "produces nothing"
          (is (nil? tx)))))))


(deftest notify-customer
  (letfn [(make-event [account status]
            (event/notify :stripe.event.customer.source/updated
                          {:params {:account-id [:account/email (account/email account)]
                                    :status     status}}))]
    (with-conn conn
      (let [account  (mock/account-tx :role :account.role/onboarding)
            {tx :tx} (helpers/dispatch-event conn (make-event account "verification_failed") account)]

        (testing "produces channel"
          (is (p/chan? tx))
          (is (map? (a/<!! tx)))))

      (let [account  (mock/account-tx)
            {tx :tx} (helpers/dispatch-event conn (make-event account "verified") account)]

        (testing "produces nothing"
          (is (nil? tx)))))))
