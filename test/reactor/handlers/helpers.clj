(ns reactor.handlers.helpers
  (:require [datomic.api :as d]
            [mock.mock :as mock]
            [reactor.models.event :as event]
            [reactor.reactor :as reactor]))

(defn deps
  "Produce event handler dependencies."
  [db & {:keys [safety mailer slack weebly stripe]}]
  (-> (reactor/deps (or safety (mock/community-safety))
                    (or mailer (mock/mailer))
                    (or slack (mock/slack))
                    (or weebly (mock/weebly))
                    (or stripe (mock/stripe))
                    "https://joinstarcity.com")
      (assoc :db db)))


(defn dispatch
  "Given a datomic `conn`, event `key`, `dispatch-fn` to process the event with
  and `opts` to pass to construct the event with, transact the event to the
  database and process it. Produces the event entity and result of `dispatch-fn`."
  [conn key dispatch-fn & {:as opts}]
  (let [uuid  (d/squuid)
        opts  (assoc opts :uuid uuid)
        db    (:db-after @(d/transact conn [(event/create key opts)]))
        event (event/by-uuid db uuid)]
    [event (dispatch-fn (deps db) event (event/params event))]))


(defn dispatch-event
  [conn event dispatch-fn & txdata]
  (let [uuid  (d/squuid)
        event (assoc event :event/uuid uuid)
        db    (:db-after (d/with (d/db conn) (conj txdata event)))
        event (event/by-uuid db uuid)
        tx    (dispatch-fn (deps db) event (event/params event))]
    {:db    db
     :event event
     :tx    tx}))
