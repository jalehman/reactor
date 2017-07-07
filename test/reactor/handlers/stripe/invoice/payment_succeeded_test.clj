(ns reactor.handlers.stripe.invoice.payment-succeeded-test
  (:require [blueprints.models.member-license :as member-license]
            [blueprints.models.order :as order]
            [blueprints.models.payment :as payment]
            [blueprints.models.rent-payment :as rent-payment]
            [blueprints.models.service :as service]
            [clj-time.coerce :as c]
            [clj-time.core :as t]
            [clojure.core.async :as a]
            [clojure.test :refer :all]
            [datomic.api :as d]
            [mock.mock :as mock]
            mock.stripe.event
            [reactor.dispatch :as dispatch]
            [reactor.fixtures :as fixtures :refer [with-conn]]
            [reactor.handlers.helpers :as helpers]
            [reactor.handlers.stripe.invoice.common :as ic]
            [reactor.handlers.stripe.invoice.payment-succeeded]
            [reactor.handlers.stripe.test-utils :as tu]
            [blueprints.models.event :as event]
            [ribbon.event :as re]
            [toolbelt.core :as tb]
            [toolbelt.datomic :as td]
            [toolbelt.predicates :as p]))

(use-fixtures :once fixtures/conn-fixture)


;; =============================================================================
;; payment succeeded
;; =============================================================================


(deftest payment-succeeded
  (let [stripe-event mock.stripe.event/invoice-payment-succeeded
        scenario     (partial tu/speculate-scenario :stripe.event.invoice/payment-succeeded stripe-event)]

    (with-conn conn
      (testing "invoices not associated with a license or order produce no transaction"
        (let [{tx :tx} (scenario conn)]
          (is (nil? tx))))

      (testing "invoices associated with rent payments"
        (let [license          (mock/member-license-tx :sub-id (ic/subs-id stripe-event))
              account          (mock/account-tx :license (td/id license))
              payment          (rent-payment/autopay-payment license (re/subject-id stripe-event)
                                                             (c/to-date (t/date-time 2017 1 1)))
              {tx :tx :as out} (scenario conn account license (member-license/add-rent-payments license payment))]

          (testing "transaction validity"
            (is (vector? tx) "produces several updates")
            (is (= 3 (count tx))))

          (testing "the rent payment is now marked as paid"
            (let [py (tb/find-by :rent-payment/status tx)]
              (is (= :rent-payment.status/paid (:rent-payment/status py)))))

          (testing "an event is created to notify the customer"
            (let [ev (tb/find-by event/notify? tx)]
              (is (= :reactor.handlers.stripe.invoice.payment-succeeded/notify.rent (event/key ev)))
              (is (= (re/subject-id stripe-event) (-> ev event/params :invoice)))
              (is (= (td/id (:event out)) (td/id (event/triggered-by ev))))))

          (testing "an event is created to notify us internally"
            (let [ev (tb/find-by event/report? tx)]
              (is (= :reactor.handlers.stripe.invoice.payment-succeeded/notify.rent (event/key ev)))
              (is (= (re/subject-id stripe-event) (-> ev event/params :invoice)))
              (is (= (td/id (:event out)) (td/id (event/triggered-by ev))))))))

      (testing "invoices associated with services"
        (let [account  (mock/account-tx)
              service  (service/customize-furniture (d/db conn))
              order    (assoc (order/create account service {:price 50.0})
                              :stripe/subs-id (ic/subs-id stripe-event))
              payment  (payment/create 50.0 :for :payment.for/order)
              payment' (merge payment (payment/add-invoice payment (re/subject-id stripe-event)))
              {tx :tx} (scenario conn account order payment')]

          (is (map? tx) "a single entity is to be updated")
          (is (payment/paid? tx) "the payment has been marked as paid"))))))


;; =============================================================================
;; notifications
;; =============================================================================


(deftest notify-internal
  (let [stripe-event mock.stripe.event/invoice-payment-succeeded
        scenario     (partial tu/speculate-scenario :stripe.event.invoice/payment-succeeded stripe-event)]

    (with-conn conn
      (let [license  (mock/member-license-tx :sub-id (ic/subs-id stripe-event))
            account  (mock/account-tx :license (td/id license))
            payment  (rent-payment/autopay-payment license (re/subject-id stripe-event)
                                                   (c/to-date (t/date-time 2017 1 1)))
            event    (event/report :reactor.handlers.stripe.invoice.payment-succeeded/notify.rent
                                   {:params {:invoice (re/subject-id stripe-event)}})
            {tx :tx} (helpers/dispatch-event conn event license account payment
                                             (member-license/add-rent-payments license payment))]
        (testing "produces a channel"
          (is (p/chan? tx))
          (is (map? (a/<!! tx))))))))


(deftest notify-customer
  (let [stripe-event mock.stripe.event/invoice-payment-succeeded
        scenario     (partial tu/speculate-scenario :stripe.event.invoice/payment-succeeded stripe-event)]

    (with-conn conn
      (let [license  (mock/member-license-tx :sub-id (ic/subs-id stripe-event))
            account  (mock/account-tx :license (td/id license))
            payment  (rent-payment/autopay-payment license (re/subject-id stripe-event)
                                                   (c/to-date (t/date-time 2017 1 1)))
            event    (event/notify :reactor.handlers.stripe.invoice.payment-succeeded/notify.rent
                                   {:params {:invoice (re/subject-id stripe-event)}})
            {tx :tx} (helpers/dispatch-event conn event license account payment
                                             (member-license/add-rent-payments license payment))]
        (testing "produces a channel"
          (is (p/chan? tx))
          (is (map? (a/<!! tx))))))))
