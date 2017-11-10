(ns reactor.seed
  (:require [blueprints.models.license :as license]
            [blueprints.seed.accounts :as accounts]
            [datomic.api :as d]
            [toolbelt.core :as tb]
            [io.rkn.conformity :as cf]))

(defn- rand-unit [property]
  (-> property :property/units vec rand-nth :db/id))


(defn distinct-by
  "Returns elements of xs which return unique values according to f. If multiple
  elements of xs return the same value under f, the first is returned"
  [f xs]
  (let [s (atom #{})]
    (for [x     xs
          :let  [id (f x)]
          :when (not (contains? @s id))]
      (do (swap! s conj id)
          x))))


(defn- accounts [db]
  (let [license  (license/by-term db 3)
        property (d/entity db [:property/internal-name "2072mission"])
        members  (->> (range 13)
                      (map (fn [_] (accounts/member (rand-unit property) (:db/id license))))
                      (distinct-by (comp :account/email #(tb/find-by :account/email %))))]
    (apply concat
           (accounts/member [:unit/name "52gilbert-1"] (:db/id license) :email "member@test.com")
           (accounts/admin :first-name "Josh" :last-name "Lehman" :email "admin@test.com")
           members)))


(defn seed [conn]
  (let [accounts-tx (accounts (d/db conn))]
    (cf/ensure-conforms conn {:seed/accounts {:txes [accounts-tx]}})))
