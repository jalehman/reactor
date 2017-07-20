(ns reactor.handlers.stripe.invoice.updated-test
  (:require [blueprints.models.member-license :as member-license]
            [blueprints.models.order :as order]
            [blueprints.models.payment :as payment]
            [blueprints.models.rent-payment :as rent-payment]
            [blueprints.models.service :as service]
            [clj-time.coerce :as c]
            [clj-time.core :as t]
            [clojure.test :refer :all]
            [datomic.api :as d]
            [mock.mock :as mock]
            mock.stripe.event
            [reactor.fixtures :as fixtures :refer [with-conn]]
            [reactor.handlers.stripe.invoice.common :as ic]
            [reactor.handlers.stripe.test-utils :as tu]
            [ribbon.event :as re]
            [toolbelt.datomic :as td]))


(use-fixtures :once fixtures/conn-fixture)


(deftest invoice-updated
  (let [stripe-event mock.stripe.event/invoice-updated
        scenario     (partial tu/speculate-scenario :stripe.event.invoice/updated stripe-event)]

    (with-conn conn
      (testing "invoices not associated with a license or order produce no transaction"
        (let [{tx :tx} (scenario conn)]
          (is (nil? tx))))

      (testing "invoices associated with rent payments"
        (let [license  (mock/member-license-tx :sub-id (ic/subs-id stripe-event))
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
                              :stripe/subs-id (ic/subs-id stripe-event))
              payment  (payment/create 50.0 account
                                       :for :payment.for/order
                                       :invoice-id (re/subject-id stripe-event))
              {tx :tx} (scenario conn account order payment)]

          (testing "transaction validity"
            (is (map? tx))
            (is (contains? tx :db/id)))

          (testing "the payment has a charge id"
            (is (-> tx :stripe/charge-id string?))))))))
