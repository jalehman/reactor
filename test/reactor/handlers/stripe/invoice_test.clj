(ns reactor.handlers.stripe.invoice-test
  (:require [clojure.test :refer :all]
            mock.stripe.event
            [reactor.handlers.stripe.invoice.common :as ic]))

(deftest subs-id
  (testing "produces a string subscription id when given an invoice event"
    (is (string? (ic/subs-id mock.stripe.event/invoice-created))))
  (testing "produces nil on another map"
    (is (nil? (ic/subs-id mock.stripe.event/failed-charge)))))
