(ns reactor.handlers.order-test
  (:require [clojure.core.async :as a]
            [clojure.test :refer :all]
            [mock.mock :as mock]
            [reactor.fixtures :as fixtures :refer [with-conn]]
            [reactor.handlers.helpers :as helpers]
            [reactor.handlers.order :as rho]
            [toolbelt.datomic :as td]
            [toolbelt.async :as ta]
            [blueprints.models.service :as service]
            [datomic.api :as d]
            [blueprints.models.order :as order]
            [blueprints.models.event :as event]
            [blueprints.models.account :as account]))


;; (use-fixtures :once fixtures/conn-fixture)


;; (defn simulate-event
;;   [deps event & txdata]
;;   (let [uuid  (d/squuid)
;;         event (assoc event :event/uuid uuid)
;;         db    (:db-after (d/with (:db deps) (conj txdata event)))
;;         event (event/by-uuid db uuid)
;;         tx    ((helpers/dispatch-for-event event) (assoc deps :db db) event (event/params event))]
;;     {:db    db
;;      :event event
;;      :tx    tx}))


;; (defn- simulate-bill-once [db]
;;   (let [account  (mock/account-tx)
;;         service  (service/customize-furniture db)
;;         stripe   (mock/stripe {:id "TESTID"})
;;         order    (order/create account service {:price 50.0})
;;         txr      (d/with db [account order])
;;         order-id (d/resolve-tempid (:db-after txr) (:tempids txr) (td/id order))
;;         deps     (helpers/deps (:db-after txr) :stripe stripe)
;;         event    (event/job :order/place {:params {:account-id [:account/email (account/email account)]
;;                                                    :order-id   order-id}})]

;;     (simulate-event deps event)))


;; (deftest order-billed-once
;;   (with-conn conn
;;     (let [{tx :tx} (simulate-bill-once (d/db conn))]
;;       (println tx)
;;       )))
