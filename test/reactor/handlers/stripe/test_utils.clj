(ns reactor.handlers.stripe.test-utils
  (:require [datomic.api :as d]
            [mock.mock :as mock]
            [reactor.dispatch :as dispatch]
            [reactor.handlers.helpers :as helpers]
            [reactor.models.event :as event]))


(defn mock-id [e]
  (:id e))


(defn mock-body [e]
  (get-in e [:data :object]))


(defn mock-amount [e]
  (:amount (mock-body e)))


(defn mock-subj [e]
  (:id (mock-body e)))


(defn speculate-scenario
  "Construct a scenario where the database has been transacted with a new Stripe
  event and `txdta`."
  [dispatch-key stripe-event conn & txdata]
  (let [event (event/create dispatch-key {:id (mock-id stripe-event) :topic :stripe})
        db    (:db-after (d/with (d/db conn) (conj txdata event)))
        deps  (helpers/deps db :stripe (mock/stripe stripe-event))
        ent   (event/by-id db (mock-id stripe-event))
        tx    (dispatch/stripe deps ent (event/params ent))]
    {:event ent
     :tx    tx
     :db    db}))
