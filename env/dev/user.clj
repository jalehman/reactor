(ns user
  (:require [blueprints.models
             [account :as account]
             [license :as license]
             [member-license :as member-license]
             [order :as order]
             [service :as service]]
            [clj-time
             [coerce :as c]
             [core :as t]]
            [clojure.spec.test :as stest]
            [clojure.tools.namespace.repl :refer [refresh]]
            [datomic.api :as d]
            [mount.core :as mount]
            [reactor.core :refer [conn]]
            [taoensso.timbre :as timbre]
            [reactor.models.event :as event]))

(timbre/refer-timbre)


(def start #(mount/start-with-args {:env :dev}))


(def stop mount/stop)


(defn go []
  (start)
  (stest/instrument)
  :ready)


(defn reset []
  (stop)
  (refresh :after 'user/go))


(comment
  @(d/transact conn [[:db.event/create ::test {}]]) ; DONE: events w/o topics

  @(d/transact conn [[:db.event/create ::test {:topic :thing}]]) ; DONE: events w/ unrecognized topics

  @(d/transact conn [[:db.event/create ::test {:topic :job}]]) ; DONE: events w/ unrecognized topics

  @(d/transact conn [[:db.event/create ::test {:topic :mail}]]) ; events w/ recognized topics

  ;; Create Account
  @(d/transact conn [[:db.event/create :account/create {:params {:email      "test@test.com"
                                                                 :first-name "Jocelyn"
                                                                 :last-name  "Robancho"
                                                                 :password   "password"}}]])

  ;; Create in-progress application
  @(d/transact conn [{:db/id               (d/tempid :db.part/starcity -1)
                      :application/address {:address/country     "US"
                                            :address/locality    "Oakland"
                                            :address/region      "CA"
                                            :address/postal-code "94611"}
                      :application/status  :application.status/in-progress}
                     {:db/id               [:account/email "test@test.com"]
                      :account/dob         (c/to-date (t/date-time 1989 1 27))
                      :account/application (d/tempid :db.part/starcity -1)}])

  ;; Submit application
  (let [application-id (-> (d/entity (d/db conn) [:account/email "test@test.com"])
                           :account/application
                           :db/id)]
    @(d/transact conn [[:db.application/submit application-id]]))


  ;; Create member license
  (let [account (d/entity (d/db conn) [:account/email "test@test.com"])]
    @(d/transact conn [{:db/id            (:db/id account)
                        :account/licenses (member-license/create (license/by-term (d/db conn) 6)
                                                                 (d/entity (d/db conn) [:unit/name "52gilbert-6"])
                                                                 (java.util.Date.)
                                                                 2200.0
                                                                 :member-license.status/active)}
                       (order/create account (service/customize-furniture (d/db conn)))]))

  ;; Promote to member
  @(d/transact conn [[:db.event/create :account/promoted {:params {:account-id [:account/email "test@test.com"]}}]])


  ;; Create collaborator
  @(d/transact conn [[:db.event/create :collaborator/create {:params {:email   "collaborator@test.com"
                                                                      :type    "real estate"
                                                                      :message "Hi! I'd like to collaborate!"}}]])

  ;; Subscribe to newsletter

  )
