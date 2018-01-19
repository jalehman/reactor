(ns reactor.hubspot-sync
  (:refer-clojure :exclude [sync])
  (:require [blueprints.models.account :as account]
            [blueprints.models.application :as application]
            [blueprints.models.sync :as sync]
            [clj-time.coerce :as c]
            [clj-time.core :as t]
            [clojure.string :as string]
            [datomic.api :as d]
            [hubspot.contact :as contact]
            [hubspot.engagement :as engagement]
            [hubspot.http :as hubspot]
            [mount.core :refer [defstate]]
            [org.httpkit.client :as http]
            [reactor.config :as config :refer [config]]
            [taoensso.timbre :as timbre]
            [toolbelt.datomic :as td]
            [toolbelt.core :as tb]
            [blueprints.models.license :as license]
            [blueprints.models.property :as property]
            [blueprints.models.address :as address]))


;; ==============================================================================
;; hubspot syncing ==============================================================
;; ==============================================================================


;; 12/19/17
;; My hunch is that this functionality belongs somewhere else, but we need
;; syncing to Hubspot in the near-term. Reactor is a running service and one of
;; the easiest to deploy, so it's a sensible place to put it for now.


;; engagement body ==============================================================

(defn content* [label content]
  [:div
   [:p [:b (string/upper-case label)]]
   [:p (or content [:i "not answered"])]])


(defmulti content-for (fn [k application] k))


(defmethod content-for :default [k application]
  (content* (name k) (get application k)))


(defmethod content-for :application/license [_ application]
  (let [license (when-some [l (application/desired-license application)]
                  (str (license/term l) " months"))]
    (content* "Desired Term" license)))


(defmethod content-for :application/communities [_ application]
  (let [communities (when-some [cs (application/communities application)]
                      (->> (map property/name cs)
                           (interpose ", ")
                           (apply str)))]
    (content* "Desired Communities" communities)))


(defmethod content-for :application/address [_ application]
  (let [address (when-some [a (application/address application)]
                  (format "%s %s, %s %s"
                          (address/locality a)
                          (address/region a)
                          (address/zip a)
                          (address/country a)))]
    (content* "Current Address" address)))


(defmethod content-for :application/pet [_ application]
  (->> (cond
         (nil? (application/has-pet? application))
         "not answered"

         (false? (application/has-pet? application))
         [:i "no pet"]

         :otherwise
         (if-let [p (application/pet application)]
           (format "Has a %slb %s (dog)." (:pet/weight p) (:pet/breed p))
           "not answered"))
       (content* "Pet")))


(defmethod content-for :application/community-fitness [_ application]
  (if (some? (application/community-fitness application))
    (map
     (fn [{:keys [label value]}]
       (content* label value))
     (application/community-fitness-labeled application))
    (content* "Community Fitness" nil)))


(defmethod content-for :application/link [_ application]
  (let [account (application/account application)]
    (->> (format "%s/accounts/%s"
                 (config/dashboard-hostname config)
                 (td/id application))
         (content* "View on Dashboard"))))


(def ^:private application-keys
  [:application/status
   :application/license
   :application/move-in
   :application/communities
   :application/address
   :application/pet
   :application/community-fitness
   :application/link
   :application/completed-at])


(defn- engagement-body
  "Generate the body for the HubSpot engagement."
  [application]
  (hiccup.core/html
   [:div
    (map #(content-for % application) application-keys)]))


;; query applications ===========================================================


(defn- query-member-applications
  "Produce a list of entities that have been modified since `modified-after`."
  [db modified-after]
  (->> (d/q '[:find [?app ...]
              :in $ ?after
              :where
              [_ :account/application ?app]
              (or-join [?app ?after]
                (and [?app _ _ ?app-tx _]
                  [?app-tx :db/txInstant ?tx-time]
                  [(.after ^java.util.Date ?tx-time ?after)])
                (and [?app :application/fitness ?fit]
                  [?fit _ _ ?fit-tx _]
                  [?fit-tx :db/txInstant ?tx-time]
                  [(.after ^java.util.Date ?tx-time ?after)]))]
            db modified-after)
       (map (partial d/entity db))))


;; hubspot syncing ==============================================================


(defn- create-contact!
  "Create the contact in hubspot."
  [db account application]
  (let [params {:phone     (account/phone-number account)
                :firstname (account/first-name account)
                :lastname  (account/last-name account)}]
    (timbre/info ::create-contact {:email (account/email account)})
    (contact/create! (account/email account) {:params params})))


(defn sync-new-applicant!
  "Sync a new applicant with hubspot."
  [conn application]
  (let [account     (application/account application)
        contact-ids (->> (or (-> (account/email account) contact/search :contacts not-empty)
                             [(create-contact! (d/db conn) account application)])
                         (map :vid))
        engagement  (engagement/create!
                     {:engagement   {:type "NOTE"}
                      :metadata     {:body (engagement-body application)}
                      :associations {:contactIds (vec contact-ids)}})]
    (timbre/info ::create-engagement {:application (td/id application)
                                      :contact-ids contact-ids})
    @(d/transact conn [(sync/create application (str (get-in engagement [:engagement :id])) :hubspot)])))


(defn sync-existing-applicant!
  "Sync an existing applicant."
  [conn sync]
  (let [application   (sync/ref sync)
        engagement-id (tb/str->int (sync/ext-id sync))]
    (engagement/update! engagement-id {:metadata {:body (engagement-body application)}})
    (timbre/info ::update-engagement {:application (td/id application)})
    @(d/transact conn [(sync/synced-now sync)])))


;; sync entrypoint(s) ===========================================================


(defn- sync-application!
  "Sync the application to HubSpot."
  [conn application]
  (try
    (let [sync (sync/by-entity (d/db conn) application)]
      (if (and (some? sync) (= (sync/service sync) :hubspot))
        (sync-existing-applicant! conn sync)
        (sync-new-applicant! conn application)))
    (catch Throwable t
      (timbre/error t ::application {:message     "Failed to sync application!"
                                     :application (td/id application)}))))


(defn- should-sync?
  "`application` should be synced if a sync entity does not exist, or the
  application has been modified since the last sync time."
  [db application]
  (let [sync (sync/by-entity db application)]
    (or (nil? sync)
        (not= (sync/service sync) :hubspot)
        (t/after? (c/to-date-time (application/last-modified-at db application))
                  (c/to-date-time (sync/last-synced sync))))))


(defn sync-applications
  "Sync all member applications with Hubspot."
  [conn]
  (let [since        (c/to-date (t/minus (t/now) (t/months 1)))
        db           (d/db conn)
        applications (->> (query-member-applications db since)
                          (filter (partial should-sync? db)))]
    ;; For each application in `applications`, perform the sync
    (timbre/info ::syncing {:count (count applications)
                            :since since})
    (doseq [a applications]
      (sync-application! conn a))))


;; state ========================================================================


(defstate hubspot
  ;; to initialize the api key
  :start (hubspot/use-api-key! (config/hubspot-api-key config)))


(comment

  (def conn reactor.datomic/conn)


  (query-member-applications (d/db reactor.datomic/conn) (c/to-date (t/minus (t/now) (t/months 1))))


  @(d/transact conn [{:db/id                   285873023223203
                      :application/move-in     (java.util.Date.)
                      :application/license     (:db/id (license/by-term (d/db conn) 3))
                      :application/communities [[:property/code "52gilbert"]
                                                [:property/code "2072mission"]]
                      :application/address     {:address/country     "US"
                                                :address/locality    "San Francisco"
                                                :address/region      "CA"
                                                :address/postal-code "94133"}
                      :application/has-pet     true
                      :application/pet         {:pet/breed "pitbull" :pet/weight 50}
                      :application/fitness     {:fitness/skills "Nunc rutrum turpis sed pede."}}])


  (sync-application! conn (d/entity (d/db conn) 285873023223203))

  (sync-applications conn)


  )
