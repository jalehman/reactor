(ns reactor.sync
  (:require [blueprints.models.application :as application]
            [blueprints.models.sync :as sync]
            [clj-time.coerce :as c]
            [clj-time.core :as t]
            [datomic.api :as d]
            [hubspot.http :as hubspot]))

;; 12/19/17
;; My hunch is that this functionality belongs somewhere else, but we need
;; syncing to Hubspot in the near-term. Reactor is a running service and one of
;; the easiest to deploy, so it's a sensible place to put it for now.


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


(comment

  (def conn reactor.datomic/conn)

  (query-member-applications (d/db reactor.datomic/conn) (c/to-date (t/minus (t/now) (t/months 1))))

  (d/entity (d/db conn) 285873023223234)

  ;; Is there a sync entity for the application?

  (sync/by-entity (d/db conn))

  )


(defn- sync-application
  [conn application]
  )


(defn- should-sync?
  "`application` should be synced if a sync entity does not exist, or the
  application has been modified since the last sync time."
  [db application]
  (let [sync (sync/by-entity db application)]
    (or (nil? sync)
        (t/after? (c/to-date-time (application/last-modified-at db application))
                  (c/to-date-time (sync/last-synced sync))))))



(defn sync-applications
  "Sync all member applications with Hubspot."
  [conn]
  (let [since        (c/to-date (t/minus (t/now) (t/months 1)))
        applications (->> (query-member-applications (d/db conn) since)
                          (filter (should-sync? (d/db conn))))]
    ))


(comment

  (query-member-applications (d/db reactor.datomic/conn) (c/to-date (t/minus (t/now) (t/months 1))))

  (t/after? (c/to-date-time #inst "2017-01-04") (c/to-date-time #inst "2017-01-02"))


  )
