(ns reactor.seed
  (:require [blueprints.models.license :as license]
            [blueprints.seed.accounts :as accounts]
            [datomic.api :as d]
            [io.rkn.conformity :as cf]
            [toolbelt.core :as tb]
            [clj-time.coerce :as c]
            [clj-time.core :as t]
            [teller.property :as tproperty]
            [teller.customer :as tcustomer]
            [blueprints.models.member-license :as member-license]
            [teller.core :as teller]
            [teller.source :as tsource]
            [teller.payment :as tpayment]
            [toolbelt.date :as date]))

(defn- rand-unit [property]
  (-> property :property/units vec rand-nth :db/id))


(defn- accounts [db]
  (let [license    (license/by-term db 3)
        property   (d/entity db [:property/internal-name "2072mission"])
        distinct   (fn [coll] (tb/distinct-by (comp :account/email #(tb/find-by :account/email %)) coll))
        members    (->> (range 13)
                        (map (fn [_] (accounts/member (rand-unit property) (:db/id license))))
                        distinct
                        (apply concat))
        applicants (->> (repeatedly accounts/applicant) (take 15) distinct)]
    (apply concat
           (accounts/member [:unit/name "52gilbert-1"] (:db/id license) :email "member@test.com")
           (accounts/admin :first-name "Josh" :last-name "Lehman" :email "admin@test.com")
           members
           applicants)))


(defn- rand-date []
  (c/to-date (t/date-time 2017 (inc (rand-int 12)) (inc (rand-int 28)))))


(defn- seed-properties [teller]
  (let [fees (tproperty/construct-fees (tproperty/format-fee 5))]
    (when-not (tproperty/by-id teller "52gilbert")
      (tproperty/create! teller "52gilbert" "52 Gilbert" "jesse@starcity.com"
                         {:fees      fees
                          :deposit   "acct_1C5LJXEd7myLyyjs"
                          :ops       "acct_1C3TmPHnEDeEkGIS"
                          :community [:property/code "52gilbert"]
                          :timezone  "America/Los_Angeles"}))
    (when-not (tproperty/by-id teller "2072mission")
      (tproperty/create! teller "2072mission" "2072 Mission" "jesse@starcity.com"
                         {:fees      fees
                          :deposit   "acct_1C3S9tD1iZkoyuLX"
                          :ops       "acct_1C3TmMEBSLaHdiO2"
                          :community [:property/code "2072mission"]
                          :timezone  "America/Los_Angeles"}))))


(def mock-visa-credit
  {:object    "card"
   :exp_month 12
   :exp_year  23
   :number    "4242424242424242"})


(defn- seed-payments [teller]
  (when (nil? (tcustomer/by-email teller "member@test.com"))
    (let [customer (tcustomer/create! teller "member@test.com"
                                      {:account  [:account/email "member@test.com"]
                                       :source   mock-visa-credit
                                       :property (tproperty/by-id teller "52gilbert")})
          tz       (t/time-zone-for-id "America/Los_Angeles")
          license (member-license/active (d/db (teller/db teller)) [:account/email "member@test.com"])]
      (tsource/set-default! (first (tcustomer/sources customer)) :payment.type/order)
      (tpayment/create! customer 2000.0 :payment.type/rent
                        {:subtypes [:fee :redicuous-fee]
                         :due    (date/end-of-day (java.util.Date.) tz)
                         :period [(date/beginning-of-month (java.util.Date.) tz)
                                  (date/end-of-month (java.util.Date.) tz)]}) "\n\n")))


(defn seed-teller [teller]
  (seed-properties teller)
  (seed-payments teller))


(defn seed [conn]
  (let [accounts-tx (accounts (d/db conn))]
    (cf/ensure-conforms conn {:seed/accounts {:txes [accounts-tx]}})))
