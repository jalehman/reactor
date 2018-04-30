(ns reactor.seed
  (:require [blueprints.models.license :as license]
            [blueprints.seed.accounts :as accounts]
            [datomic.api :as d]
            [io.rkn.conformity :as cf]
            [toolbelt.core :as tb]))

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


(defn seed [conn]
  (let [accounts-tx (accounts (d/db conn))]
    (cf/ensure-conforms conn {:seed/accounts {:txes [accounts-tx]}})))
