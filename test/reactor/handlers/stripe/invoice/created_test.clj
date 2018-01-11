(ns reactor.handlers.stripe.invoice.created-test
  (:require [blueprints.models.account :as account]
            [blueprints.models.event :as event]
            [blueprints.models.member-license :as member-license]
            [blueprints.models.order :as order]
            [blueprints.models.payment :as payment]
            [blueprints.models.service :as service]
            [clojure.core.async :as a]
            [clojure.test :refer :all]
            [datomic.api :as d]
            [mock.mock :as mock]
            mock.stripe.event
            [reactor.fixtures :as fixtures :refer [with-conn]]
            [reactor.handlers.helpers :as helpers]
            [reactor.handlers.stripe.invoice.common :as ic]
            reactor.handlers.stripe.invoice.created
            [reactor.handlers.stripe.test-utils :as tu]
            [ribbon.event :as re]
            [toolbelt.async :as ta]
            [toolbelt.core :as tb]
            [toolbelt.datomic :as td]))

;; =============================================================================
;; invoice.created
;; =============================================================================


(deftest invoice-created
  (let [event    mock.stripe.event/invoice-created
        scenario (partial tu/speculate-scenario :stripe.event.invoice/created event)]

    (with-conn conn
      (testing "invoices not associated with a license or order produce no transaction"
        (let [{tx :tx} (scenario conn)]
          (is (nil? tx))))


      (testing "invoices associated with rent payments"
        (let [license          (mock/member-license-tx :sub-id (ic/subs-id event))
              account          (mock/account-tx
                                :role :account.role/member
                                :license (td/id license))
              {tx :tx :as out} (scenario conn account license)]

          (testing "transaction shape"
            (is (vector? tx))
            (is (= 2 (count tx))))

          (testing "a rent payment is added to the member license"
            (let [py (first (:member-license/rent-payments (tb/find-by :member-license/rent-payments tx)))]
              (is (td/entity? py))
              (is (= (payment/invoice-id py) (re/subject-id event))
                  "the invoice id made it onto the payment")))

          (testing "an event to notify the customer is created"
            (let [ev (tb/find-by event/notify? tx)]
              (is (= :reactor.handlers.stripe.invoice.created/notify.rent (event/key ev)))
              (is (= (td/id (:event out)) (-> ev event/triggered-by td/id)))))))

      (testing "invoices associated with premium service orders"
        (let [account          (mock/account-tx)
              service          (service/customize-furniture (d/db conn))
              order            (assoc (order/create account service
                                                    {:price  50.0
                                                     :status :order.status/placed})
                                      :stripe/subs-id (ic/subs-id event))
              {tx :tx :as out} (scenario conn account order)]

          (testing "transaction validity"
            (is (vector? tx))
            (is (= 3 (count tx))))

          (testing "an event is created to notify the customer"
            (let [ev (tb/find-by event/notify? tx)]
              (is (= (td/id (:event out)) (event/triggered-by ev)))
              (is (= :reactor.handlers.stripe.invoice.created/notify.service (event/key ev)))))

          (testing "the order is updated with a payment"
            (let [order (tb/find-by :order/payments tx)]
              (is (some? (order/payments order)))))

          (testing "the payment is valid"
            (let [py (tb/find-by :payment/amount tx)]
              (is (> (payment/amount py) 0))
              (is (= (order/computed-price order) (payment/amount py))
                  "the payment has the same price as the order")
              (is (= (:payment/for py) :payment.for/order))
              (is (= (re/subject-id event) (payment/invoice-id py)))
              (is (payment/invoice? py))
              (is (nil? (payment/charge-id py)) "the charge is not yet created")))


          (testing "the transaction data can be transacted"
            (is (d/with (:db out) tx)))))

      (testing "invoices that are closed produce no transaction"
        (let [event    (assoc-in event [:data :object :closed] true)
              {tx :tx} (tu/speculate-scenario :stripe.event.invoice/created event conn)]
          (is (nil? tx)))))))


(deftest send-rent-email
  (with-conn conn
    (testing "produces a channel"
      (let [account  (mock/account-tx)
            event    (event/notify :reactor.handlers.stripe.invoice.created/notify.rent
                                   {:params {:account-id [:account/email (account/email account)]}})
            {tx :tx} (helpers/dispatch-event conn event account)]
        (is (ta/chan? tx))
        (is (map? (a/<!! tx)))))))


(deftest send-service-email
  (let [stripe-event mock.stripe.event/invoice-created]
    (with-conn conn
      (let [account (mock/account-tx)
            service (service/customize-furniture (d/db conn))
            event   (event/notify :reactor.handlers.stripe.invoice.created/notify.service
                                  {:params {:subs-id (ic/subs-id stripe-event)}})]

        (testing "orders must have prices"
          (let [order (assoc (order/create account service)
                             :stripe/subs-id (ic/subs-id stripe-event))]
            (is (thrown-with-msg? java.lang.AssertionError #"Order has no price"
                                  (helpers/dispatch-event conn event account order)))))

        (testing "produces a channel"
          (let [order    (assoc (order/create account service {:price 10.0})
                                :stripe/subs-id (ic/subs-id stripe-event))
                {tx :tx} (helpers/dispatch-event conn event account order)]
            (is (ta/chan? tx))
            (is (map? (a/<!! tx)))))))))
