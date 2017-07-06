(ns reactor.handlers.newsletter-test
  (:require [reactor.handlers.newsletter]
            [clojure.test :refer :all]
            [reactor.fixtures :as fixtures :refer [with-conn]]
            [reactor.handlers.helpers :as helpers]
            [reactor.handlers.newsletter]
            [reactor.dispatch :as dispatch]
            [toolbelt.predicates :as p]))


(use-fixtures :once fixtures/conn-fixture)


(deftest subscribe-to-newsletter
  (with-conn conn
    (testing "can subscribe to newsletter"
      (let [[ev tx] (helpers/dispatch conn :newsletter/subscribe :params {:email "test@test.com"})]
        (is (p/chan? tx))))))
