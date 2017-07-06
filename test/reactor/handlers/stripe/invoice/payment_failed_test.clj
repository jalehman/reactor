(ns reactor.handlers.stripe.invoice.payment-failed-test
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
            [reactor.handlers.stripe.invoice.payment-failed]
            [reactor.handlers.stripe.test-utils :as tu]
            [reactor.models.event :as event]
            [ribbon.event :as re]
            [toolbelt.core :as tb]
            [toolbelt.datomic :as td]))


(use-fixtures :once fixtures/conn-fixture)


;; =============================================================================
;; Payment Failed
;; =============================================================================


(deftest payment-failed
  (let [stripe-event mock.stripe.event/invoice-payment-failed
        scenario     (partial tu/speculate-scenario :stripe.event.invoice/payment-failed stripe-event)]

    (with-conn conn
      (testing "invoices not associated with a license or order produce no transaction"
        (let [{tx :tx} (scenario conn)]
          (is (nil? tx))))

      (testing "invoices associated with rent payments"
        (let [license (mock/member-license-tx :sub-id (ic/subs-id stripe-event))
              account (mock/account-tx :license (td/id license))
              payment (rent-payment/autopay-payment license (re/subject-id stripe-event)
                                                    (c/to-date (t/date-time 2017 1 1)))]

          (testing "invoices that have not exceeded the maximum number of attempts"
            (let [{tx :tx :as out} (scenario conn account license (member-license/add-rent-payments license payment))]

              (testing "transaction shape"
                (is (vector? tx))
                (is (= 2 (count tx))))

              (testing "will notify internally"
                (let [ev (tb/find-by event/report? tx)]
                  (is (= (re/subject-id stripe-event) (-> ev event/params :invoice)))
                  (is (= (td/id (:event out)) (td/id (event/triggered-by ev))))
                  (is (= :reactor.handlers.stripe.invoice.payment-failed/notify.rent (event/key ev)))))

              (testing "will notify customer"
                (let [ev (tb/find-by event/notify? tx)]
                  (is (= (re/subject-id stripe-event) (-> ev event/params :invoice)))
                  (is (= (td/id (:event out)) (td/id (event/triggered-by ev))))
                  (is (= :reactor.handlers.stripe.invoice.payment-failed/notify.rent (event/key ev)))))))


          (testing "invoices that HAVE exceeded the maximum number of attempts"
            (let [stripe-event     (assoc-in stripe-event [:data :object :attempt_count] rent-payment/max-autopay-failures)
                  scenario         (partial tu/speculate-scenario :stripe.event.invoice/payment-failed stripe-event)
                  {tx :tx :as out} (scenario conn account license (member-license/add-rent-payments license payment))]

              (testing "transaction shape"
                (is (vector? tx))
                (is (= 2 (count tx))))

              (testing "will retract payment"
                (let [[_ v] (tb/find-by (comp (partial = :db.fn/retractEntity) first) tx)]
                  (is (integer? v))))

              (testing "will create a new payment"
                (let [py (tb/find-by :rent-payment/status tx)]
                  (is (= :rent-payment.status/due (rent-payment/status py)))
                  (is (= (rent-payment/period-start py) (rent-payment/period-start payment)))
                  (is (= (rent-payment/period-end py) (rent-payment/period-end payment)))
                  (is (= (rent-payment/due-date py) (rent-payment/due-date payment)))
                  (is (= (rent-payment/amount py) (rent-payment/amount payment)))))))))

      (testing "invoices associated with a service order"
        (let [account  (mock/account-tx)
              service  (service/customize-furniture (d/db conn))
              order    (assoc (order/create account service {:price 50.0})
                              :stripe/subs-id (ic/subs-id stripe-event))
              payment  (payment/create 50.0 :for :payment.for/order)
              payment' (merge payment (payment/add-invoice payment (re/subject-id stripe-event)))]

          (testing "invoices that have NOT exceeded the maximum number of attempts"
            (let [{tx :tx :as out} (scenario conn account order payment')]

              (testing "transaction shape"
                (is (vector? tx))
                (is (= 3 (count tx))))

              (testing "payment is failed"
                (let [py (tb/find-by :payment/status tx)]
                  (is (payment/failed? py))))


              (testing "will notify internally"
                (let [ev (tb/find-by event/report? tx)]
                  (is (= (re/subject-id stripe-event) (-> ev event/params :invoice)))
                  (is (= (td/id (:event out)) (td/id (event/triggered-by ev))))
                  (is (= :reactor.handlers.stripe.invoice.payment-failed/notify.service (event/key ev)))))

              (testing "will notify customer"
                (let [ev (tb/find-by event/notify? tx)]
                  (is (= (re/subject-id stripe-event) (-> ev event/params :invoice)))
                  (is (= (td/id (:event out)) (td/id (event/triggered-by ev))))
                  (is (= :reactor.handlers.stripe.invoice.payment-failed/notify.service (event/key ev)))))))

          (testing "invoices that HAVE exceeded the maximum number of attempts"
            (let [stripe-event     (assoc-in stripe-event [:data :object :attempt_count] rent-payment/max-autopay-failures)
                  scenario         (partial tu/speculate-scenario :stripe.event.invoice/payment-failed stripe-event)
                  {tx :tx :as out} (scenario conn account order payment')]

              (testing "transaction shape"
                (is (vector? tx))
                (is (= 3 (count tx))))

              (testing "payment is failed"
                (let [py (tb/find-by :payment/status tx)]
                  (is (payment/failed? py))))

              (testing "will notify internally"
                (let [ev (tb/find-by event/report? tx)]
                  (is (= (re/subject-id stripe-event) (-> ev event/params :invoice)))
                  (is (= (td/id (:event out)) (td/id (event/triggered-by ev))))
                  (is (= :reactor.handlers.stripe.invoice.payment-failed/notify.service.final
                         (event/key ev)))))

              (testing "will notify customer"
                (let [ev (tb/find-by event/notify? tx)]
                  (is (= (re/subject-id stripe-event) (-> ev event/params :invoice)))
                  (is (= (td/id (:event out)) (td/id (event/triggered-by ev))))
                  (is (= :reactor.handlers.stripe.invoice.payment-failed/notify.service.final
                         (event/key ev))))))))))))
