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
            [reactor.models.event :as event]
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

        (testing "transaction contains a notify-internal event"
          (let [ev' (tb/find-by (comp #{:stripe.event.customer.source.updated/notify-internal} :event/key) tx)]
            (is (= :slack (event/topic ev')))
            (is (= (td/id event) (-> ev' event/triggered-by td/id))
                "has triggered-by set")

            (let [{:keys [account-id status]} (event/params ev')]
              (is (integer? account-id))
              (is (= "verification_failed" status)))))


        (testing "transaction contains a notify-customer event"
          (let [ev' (tb/find-by (comp #{:stripe.event.customer.source.updated/notify-customer} :event/key) tx)]
            (is (= :mail (event/topic ev')))
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

        (testing "transaction contains a notify-internal events"
          (let [ev' (tb/find-by (comp #{:stripe.event.customer.source.updated/notify-internal} :event/key) tx)]
            (is (= :slack (event/topic ev')))
            (is (= (td/id event) (-> ev' event/triggered-by td/id))
                "has triggered-by set")

            (let [{:keys [account-id status]} (event/params ev')]
              (is (integer? account-id))
              (is (= "verified" status)))))

        (testing "transaction contains a notify-customer event"
          (let [ev' (tb/find-by (comp #{:stripe.event.customer.source.updated/notify-customer} :event/key) tx)]
            (is (= :mail (event/topic ev')))
            (is (= (td/id event) (-> ev' event/triggered-by td/id))
                "has triggered-by set")))))))


(deftest notify-internal
  (letfn [(make-event [account status]
            (event/create :stripe.event.customer.source.updated/notify-internal
                          {:params {:account-id [:account/email (account/email account)]
                                    :status     status}
                           :topic  :slack}))]
    (with-conn conn
      (let [account  (mock/account-tx)
            {tx :tx} (helpers/dispatch-event conn (make-event account "verification_failed") dispatch/slack account)]

        (testing "produces channel"
          (is (p/chan? tx))
          (is (map? (a/<!! tx)))))

      (let [account  (mock/account-tx)
            {tx :tx} (helpers/dispatch-event conn (make-event account "verified") dispatch/slack account)]

        (testing "produces channel"
          (is (p/chan? tx))
          (is (map? (a/<!! tx)))))

      (let [account  (mock/account-tx)
            {tx :tx} (helpers/dispatch-event conn (make-event account "blahblahblah") dispatch/slack account)]

        (testing "produces nothing"
          (is (nil? tx)))))))


(deftest notify-customer
  (letfn [(make-event [account status]
            (event/create :stripe.event.customer.source.updated/notify-customer
                          {:params {:account-id [:account/email (account/email account)]
                                    :status     status}
                           :topic  :mail}))]
    (with-conn conn
      (let [account  (mock/account-tx :role :account.role/onboarding)
            {tx :tx} (helpers/dispatch-event conn (make-event account "verification_failed") dispatch/mail account)]

        (testing "produces channel"
          (is (p/chan? tx))
          (is (map? (a/<!! tx)))))

      (let [account  (mock/account-tx)
            {tx :tx} (helpers/dispatch-event conn (make-event account "verified") dispatch/mail account)]

        (testing "produces nothing"
          (is (nil? tx)))))))
