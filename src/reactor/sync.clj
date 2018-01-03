(ns reactor.sync
  (:require [mount.core :refer [defstate]]
            [clojure.core.async :as a]
            [datomic.api :as d]
            [clj-time.core :as t]
            [clj-time.coerce :as c]))

;; 12/19/17
;; My hunch is that this functionality belongs somewhere else, but we need
;; syncing to Hubspot in the near-term. Reactor is a running service and one of
;; the easiest to deploy, so it's a sensible place to put it for now.


;; (defstate )

;; What should happen?

;; 1. hubspot-sync should be a piece of state that can `start` syncing

;; (defprotocol SyncJob
;;   "Interface to a sync job..."
;;   (start [this] "Start syncing.")
;;   (stop [this] "Stop syncing."))


;; (defrecord HubspotSync [c]
;;   SyncJob
;;   (start [this]
;;     (a/put! c )))


;; 0. We'll need to be able to convert an application into a string of text to
;; enter into the engagement.

;; TODO:
(defn- engagement-body
  [application]
  )


;; 1. Query all member applications that were modified within the last month


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


;; 2. For each member application, check to see if a `sync` entity exists:
;;   - if so, get the `external-id` and update the engagement
;;   - if not, create an engagement and `sync` entity

;; TODO:
(defn sync-application
  [conn application]
  )


(comment

  (query-member-applications (d/db reactor.datomic/conn) (c/to-date (t/minus (t/now) (t/months 1))))

  )
