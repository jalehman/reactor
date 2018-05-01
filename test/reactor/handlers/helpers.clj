(ns reactor.handlers.helpers
  (:require [datomic.api :as d]
            [mock.mock :as mock]
            [blueprints.models.event :as event]
            [reactor.deps :as deps]
            [reactor.dispatch :as dispatch]))


(defn deps
  "Produce event handler dependencies."
  [db & {:keys [safety mailer slack stripe]}]
  (-> {:community-safety (or safety (mock/community-safety))
       :mailer           (or mailer (mock/mailer))
       :slack            (or slack (mock/slack))
       :public-hostname  "https://starcity.com"}
      (assoc :db db)))


(defn dispatch-for-event [event]
  (get {:report dispatch/report
        :notify dispatch/notify
        :stripe dispatch/stripe
        :job    dispatch/job}
       (event/topic event)
       dispatch/job))


(defn dispatch
  "Given a datomic `conn`, event `key`, `dispatch-fn` to process the event with
  and `opts` to pass to construct the event with, transact the event to the
  database and process it. Produces the event entity and result of `dispatch-fn`."
  [conn key & {:as opts}]
  (let [uuid  (d/squuid)
        opts  (assoc opts :uuid uuid)
        db    (:db-after (d/with (d/db conn) [(event/create key opts)]))
        event (event/by-uuid db uuid)]
    [event ((dispatch-for-event event) (deps db) event (event/params event))]))


(defn dispatch-event
  [conn event & txdata]
  (let [uuid  (d/squuid)
        event (assoc event :event/uuid uuid)
        db    (:db-after (d/with (d/db conn) (conj txdata event)))
        event (event/by-uuid db uuid)
        tx    ((dispatch-for-event event) (deps db) event (event/params event))]
    {:db    db
     :event event
     :tx    tx}))
