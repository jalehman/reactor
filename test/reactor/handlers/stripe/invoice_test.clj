(ns reactor.handlers.stripe.invoice-test
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
            [reactor.handlers.stripe.invoice :as invoice]
            [reactor.handlers.stripe.test-utils :as tu]
            [reactor.models.event :as event]
            [ribbon.event :as re]
            [toolbelt
             [core :as tb]
             [datomic :as td]
             [predicates :as p]]
            [blueprints.models.rent-payment :as rent-payment]
            [blueprints.models.order :as order]
            [blueprints.models.service :as service]
            [datomic.api :as d]
            [clj-time.coerce :as c]
            [clj-time.core :as t]
            [blueprints.models.member-license :as member-license]
            [blueprints.models.payment :as payment]))


(use-fixtures :once fixtures/conn-fixture)


;; =============================================================================
;; Tests
;; =============================================================================


(deftest subs-id
  (testing "produces a string subscription id when given an invoice event"
    (is (string? (invoice/subs-id mock.stripe.event/invoice-created))))
  (testing "produces nil on another map"
    (is (nil? (invoice/subs-id mock.stripe.event/failed-charge)))))


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
        (let [license          (mock/member-license-tx :sub-id (invoice/subs-id event))
              account          (mock/account-tx
                                :role :account.role/member
                                :license (td/id license))
              {tx :tx :as out} (scenario conn account license)]

          (testing "transaction validity"
            (is (vector? tx))
            (is (= 2 (count tx))))

          (testing "a rent payment is added to the member license"
            (let [py (first (:member-license/rent-payments (tb/find-by :member-license/rent-payments tx)))]
              (is (p/entity? py))
              (is (= (rent-payment/invoice py) (re/subject-id event))
                  "the invoice id made it onto the payment")))

          (testing "an event to send an email is create"
            (let [ev (tb/find-by :event/key tx)]
              (is (= :stripe.event.invoice.created.rent/send-email (event/key ev)))
              (is (= (td/id (:event out)) (-> ev event/triggered-by td/id)))
              (is (= :mail (event/topic ev)))))))

      (testing "invoices associated with premium service orders"
        (let [account          (mock/account-tx)
              service          (service/customize-furniture (d/db conn))
              order            (assoc (order/create account service {:price 50.0})
                                      :stripe/subs-id (invoice/subs-id event))
              {tx :tx :as out} (scenario conn account order)]

          (testing "transaction validity"
            (is (vector? tx))
            (is (= 3 (count tx))))

          (testing "an event is created to send an email"
            (let [ev (tb/find-by :event/key tx)]
              (is (= :mail (event/topic ev)))
              (is (= (td/id (:event out)) (event/triggered-by ev)))
              (is (= :stripe.event.invoice.created.service/send-email
                     (event/key ev)))))

          (testing "the order is updated with a payment"
            (let [order (tb/find-by :order/payments tx)]
              (is (= (order/status order) :order.status/placed))
              (is (some? (order/payments order)))))

          (testing "the payment is valid"
            (let [py (tb/find-by :payment/amount tx)]
              (is (> (payment/amount py) 0))
              (is (= (order/price order) (payment/amount py))
                  "the payment has the same price as the order")
              (is (= (payment/payment-for py) :payment.for/order))
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
            event    (event/create :stripe.event.invoice.created.rent/send-email
                                   {:params {:account-id [:account/email (account/email account)]}
                                    :topic  :mail})
            {tx :tx} (helpers/dispatch-event conn event dispatch/mail account)]
        (is (p/chan? tx))
        (is (map? (a/<!! tx)))))))


(deftest send-service-email
  (let [stripe-event mock.stripe.event/invoice-created]
    (with-conn conn
      (let [account (mock/account-tx)
            service (service/customize-furniture (d/db conn))
            event   (event/create :stripe.event.invoice.created.service/send-email
                                  {:params {:subs-id (invoice/subs-id stripe-event)}
                                   :topic  :mail})]

        (testing "orders must have prices"
          (let [order (assoc (order/create account service)
                             :stripe/subs-id (invoice/subs-id stripe-event))]
            (is (thrown-with-msg? java.lang.AssertionError #"Order has no price"
                                  (helpers/dispatch-event conn event dispatch/mail account order)))))

        (testing "produces a channel"
          (let [order    (assoc (order/create account service {:price 10.0})
                                :stripe/subs-id (invoice/subs-id stripe-event))
                {tx :tx} (helpers/dispatch-event conn event dispatch/mail account order)]
            (is (p/chan? tx))
            (is (map? (a/<!! tx)))))))))


;; =============================================================================
;; invoice.updated
;; =============================================================================


(deftest invoice-updated
  (let [stripe-event mock.stripe.event/invoice-updated
        scenario     (partial tu/speculate-scenario :stripe.event.invoice/updated stripe-event)]

    (with-conn conn
      (testing "invoices not associated with a license or order produce no transaction"
        (let [{tx :tx} (scenario conn)]
          (is (nil? tx))))

      (testing "invoices associated with rent payments"
        (let [license  (mock/member-license-tx :sub-id (invoice/subs-id stripe-event))
              account  (mock/account-tx :license (td/id license))
              payment  (rent-payment/autopay-payment license (re/subject-id stripe-event)
                                                     (c/to-date (t/date-time 2017 1 1)))
              {tx :tx} (scenario conn account license (member-license/add-rent-payments license payment))]

          (is (map? tx) "produces a single entity update")
          (is (= (-> tx :rent-payment/charge :charge/stripe-id)
                 (:charge (re/subject stripe-event)))
              "the charge id is added to the charge entity on the rent payment")))

      (testing "invoices associated with service order payments"
          (let [account  (mock/account-tx)
                service  (service/customize-furniture (d/db conn))
                order    (assoc (order/create account service {:price 50.0})
                                :stripe/subs-id (invoice/subs-id stripe-event))
                payment  (payment/create 50.0 :for :payment.for/order)
                payment' (merge payment (payment/add-invoice payment (re/subject-id stripe-event)))
                {tx :tx} (scenario conn account order payment')]

            (testing "transaction validity"
              (is (map? tx))
              (is (contains? tx :db/id)))

            (testing "the payment has a charge id"
              (is (-> tx :stripe/charge-id string?))))))))


;; =============================================================================
;; invoice.payment_succeeded
;; =============================================================================


(deftest payment-succeeded
  (let [stripe-event mock.stripe.event/invoice-payment-succeeded
        scenario     (partial tu/speculate-scenario :stripe.event.invoice/payment-succeeded stripe-event)]

    (with-conn conn
      (testing "invoices not associated with a license or order produce no transaction"
        (let [{tx :tx} (scenario conn)]
          (is (nil? tx))))

      ;; TODO: test that rent payment is paid, and two events are created.
      (testing ""))))
