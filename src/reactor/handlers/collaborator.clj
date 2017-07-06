(ns reactor.handlers.collaborator
  (:require [blueprints.models.account :as account]
            [reactor.dispatch :as dispatch]
            [reactor.handlers.common :refer :all]
            [reactor.models.event :as event]))


(defn- create-collaborator [db email]
  (if-let [account (account/by-email db email)]
    {:db/id (:db/id account)}
    (account/collaborator email)))


(defmethod dispatch/job :collaborator/create
  [deps event {:keys [email type message]}]
  (let [subject (format "Collaboration request from: %s, %s" email type)]
    [(create-collaborator (->db deps) email)
     (event/create :note/create
                   {:params       {:subject    subject
                                   :content    message
                                   :notify?    true
                                   :account-id [:account/email email]}
                    :triggered-by event})]))
