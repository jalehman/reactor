(ns reactor.handlers.session
  (:require [reactor.dispatch :as dispatch]
            [reactor.handlers.common :refer :all]
            [datomic.api :as d]))


(defn- get-session-id
  [db account-id]
  (d/q '[:find ?s .
         :in $ ?a
         :where
         [?s :session/account ?a]]
       db account-id))


(defmethod dispatch/job :session/revoke [deps event {:keys [account-id]}]
  (when-some [eid (get-session-id (->db deps) account-id)]
    [:db.fn/retractEntity eid]))
