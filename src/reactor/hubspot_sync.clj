(ns reactor.hubspot-sync
  (:refer-clojure :exclude [sync])
  (:require [blueprints.models.account :as account]
            [blueprints.models.address :as address]
            [blueprints.models.application :as application]
            [blueprints.models.license :as license]
            [blueprints.models.property :as property]
            [blueprints.models.sync :as sync]
            [clj-time.coerce :as c]
            [clj-time.core :as t]
            [clojure.string :as string]
            [datomic.api :as d]
            [hiccup.core :as html]
            [hubspot.contact :as contact]
            [hubspot.engagement :as engagement]
            [hubspot.http :as hubspot]
            [mount.core :refer [defstate]]
            [reactor.config :as config :refer [config]]
            [taoensso.timbre :as timbre]
            [toolbelt.core :as tb]
            [toolbelt.datomic :as td]))

;; ==============================================================================
;; hubspot syncing ==============================================================
;; ==============================================================================


;; 12/19/17
;; My hunch is that this functionality belongs somewhere else, but we need
;; syncing to Hubspot in the near-term. Reactor is a running service and one of
;; the easiest to deploy, so it's a sensible place to put it for now.


(def ^:private application-keys
  [:application/status
   :application/license
   :application/move-in
   :application/communities
   :application/address
   :application/pet
   :application/created
   :application/updated
   :application/completed-at
   :application/referral])


(defmulti application-param (fn [_ _ k] k))


(defmethod application-param :default [_ _ _] nil)


(defmethod application-param :application/status [_ application key]
  [:application_status (name (key application))])


(defmethod application-param :application/license [_ application key]
  (when-let [l (key application)]
    [:desired_term (format "%s months" (:license/term l))]))


(defmethod application-param :application/move-in [_ application key]
  (when-let [date (key application)]
    [:move_in (str date)]))


(defmethod application-param :application/communities [_ application _]
  (when-let [cs (application/communities application)]
    [:desired_communities
     (->> (map property/name cs)
          (interpose ", ")
          (apply str))]))


(defmethod application-param :application/address [_ application key]
  (when-let [a (application/address application)]
    [:current_address (format "%s %s, %s %s"
                              (address/locality a)
                              (address/region a)
                              (address/zip a)
                              (address/country a))]))


(defmethod application-param :application/pet [_ application key]
  (when-not (nil? (application/has-pet? application))
    (let [x (if (false? (application/has-pet? application))
              "No"
              "Dog")]
      [:pets x])))


(defmethod application-param :application/created [db application _]
  [:application_created_started (str (td/created-at db application))])


(defmethod application-param :application/updated [db application _]
  [:application_activity (str (td/updated-at db application))])


(defmethod application-param :application/completed-at [db application _]
  (when-let [d (d/q '[:find ?tx-time .
                      :in $ ?a
                      :where
                      [?a :application/status :application.status/submitted ?tx]
                      [?tx :db/txInstant ?tx-time]]
                    db (td/id application))]
    [:application_submitted (str d)]))


(defmethod application-param :application/referral [_ _ _]
  [:additional_source "Direct Application"])


(defn- application-contact-params
  [db application]
  (reduce
   (fn [acc key]
     (let [[k v] (application-param db application key)]
       (tb/assoc-when acc k v)))
   {}
   application-keys))


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
  (let [params (merge
                {:phone     (account/phone-number account)
                 :firstname (account/first-name account)
                 :lastname  (account/last-name account)}
                (application-contact-params db application))]
    (timbre/info ::create-contact {:email (account/email account)})
    (contact/create! (account/email account) params)))


(defn sync-new-applicant!
  "Sync a new applicant with hubspot."
  [conn application]
  (let [account     (application/account application)
        contact-ids (->> (or (-> (account/email account) contact/search :contacts not-empty)
                             [(create-contact! (d/db conn) account application)])
                         (map :vid))]
    (timbre/info ::sync-application {:application (td/id application)
                                     :contact-ids contact-ids})
    @(d/transact conn [(sync/create application "" :hubspot)])))


(defn sync-existing-applicant!
  "Sync an existing applicant."
  [conn sync]
  (let [application (sync/ref sync)
        account     (application/account application)]
    (contact/update! (account/email account) (application-contact-params (d/db conn) application))
    (timbre/info ::sync-application {:application (td/id application)})
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


  @(d/transact conn [{:db/id                   285873023223234
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
                      :application/status      :application.status/submitted
                      :application/fitness     {:fitness/skills "Nunc rutrum turpis sed pede."}}])


  (sync-application! conn (d/entity (d/db conn) 285873023223203))

  (sync-applications conn)


  )
