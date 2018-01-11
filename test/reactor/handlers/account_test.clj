(ns reactor.handlers.account-test
  (:require [blueprints.models
             [event :as event]
             [license :as license]
             [member-license :as member-license]
             [service :as service]
             [order :as order]]
            [clojure.spec.alpha :as s]
            [clojure.test :refer :all]
            [clojure.core.async :as a]
            [datomic.api :as d]
            [reactor
             [dispatch :as dispatch]
             [fixtures :as fixtures :refer [with-conn]]]
            [reactor.handlers.account]
            [reactor.handlers.helpers :as helpers]
            [toolbelt
             [core :as tb]
             [async :as ta]]))

(use-fixtures :once fixtures/conn-fixture)


(s/def :account/email string?)
(s/def :account/first-name string?)
(s/def :account/middle-name string?)
(s/def :account/last-name string?)
(s/def :account/activation-hash string?)
(s/def :account/activated boolean?)
(s/def :account/role #{:account.role/applicant})
(s/def ::account-tx
  (s/keys :req [:account/email
                :account/first-name
                :account/last-name
                :account/password
                :account/activation-hash
                :account/activated
                :account/role]
          :opt [:account/middle-name]))


(deftest account-creation
  (with-conn conn
    (let [params  {:email       "test@test.com"
                   :first-name  "Jocelyn"
                   :middle-name "Marie"
                   :last-name   "Robancho"
                   :password    "password"}
          [ev tx] (helpers/dispatch conn :account/create :params params)]

      (testing "produces a valid transaction"
        (is (vector? tx))
        (is (= 2 (count tx)) "two entity maps"))

      (testing "the transaction includes a valid account transaction"
        (let [account-tx (tb/find-by #(contains? % :account/email) tx)]
          (is (false? (:account/activated account-tx))
              "Accounts start unactivated")
          (is (s/valid? ::account-tx account-tx)
              "It has the correct shape.")
          (is (contains? account-tx :account/middle-name))))

      (testing "activation email event"
        (let [ev-tx (tb/find-by event/notify? tx)]
          (testing "the transaction produces an activation email event"
            (is (= (event/key ev-tx) :account/create))
            (is (= (:db/id ev) (:event/triggered-by ev-tx))
                "The activation email event is triggered by the parent event."))

          (testing "can send an activation email"
            (let [db  (:db-after @(d/transact conn tx))
                  ev  (event/by-uuid db (:event/uuid ev-tx))
                  res (dispatch/notify (helpers/deps db) ev (event/params ev))]
              (is (ta/chan? res) "produces a channel.")
              (is (map? (a/<!! res)) "the channel holds a map."))))))

    (testing "parameter validity"
      (testing "middle name is optional"
        (let [tx (second (helpers/dispatch conn :account/create :params {:email      "test@test.com"
                                                                                            :first-name "Jocelyn"
                                                                                            :last-name  "Robancho"
                                                                                            :password   "password"}))]
          (is (nil? (:account/middle-name tx)))))

      (testing "first name, last name, email and password are required"
        (let [params   {:email "test@test.com" :first-name "Jocelyn" :last-name "Robancho" :password "password"}
              dispatch #(second (helpers/dispatch conn :account/create :params %))]
          (is (thrown? java.lang.AssertionError (dispatch (dissoc params :email))))
          (is (thrown? java.lang.AssertionError (dispatch (dissoc params :first-name))))
          (is (thrown? java.lang.AssertionError (dispatch (dissoc params :last-name))))
          (is (thrown? java.lang.AssertionError (dispatch (dissoc params :password)))))))))


(defn- create-test-account [conn]
  (let [[_ account-tx] (helpers/dispatch conn :account/create
                                         :params {:email      "test@test.com"
                                                  :first-name "Jocelyn"
                                                  :last-name  "Robancho"
                                                  :password   "password"})]
    @(d/transact conn account-tx)
    (d/entity (d/db conn) [:account/email "test@test.com"])))


(defn- create-sample-member-license [conn account]
  @(d/transact conn [{:db/id            (:db/id account)
                      :account/licenses (member-license/create (license/by-term (d/db conn) 6)
                                                               (d/entity (d/db conn) [:unit/name "52gilbert-6"])
                                                               (java.util.Date.)
                                                               2200.0
                                                               :member-license.status/active)}
                     (order/create account (service/customize-furniture (d/db conn)))]))


(deftest account-promotion
  (with-conn conn
    (let [account (create-test-account conn)
          params  {:account-id (:db/id account)}]

      (testing "cannot promote member that has no active license"
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Member has no active license!"
                              (helpers/dispatch conn :account/promoted :params params))))

      (do
        (create-sample-member-license conn account)
        (let [[ev tx] (helpers/dispatch conn :account/promoted :params params)]

          (testing "valid transaction data is produced"
            (is (vector? tx))
            (is (= 3 (count tx)) "Three entity maps are produced.")
            (is (every? #(contains? % :event/key) tx)
                "All entities are events.")
            (is (every? #(= (:event/triggered-by %) (:db/id ev)) tx)
                "All events have the correct reference to the triggering event.")
            (is (every? #(= (:event/params %) (pr-str params)) tx)
                "All events have the appropriate parameters.")))))))
