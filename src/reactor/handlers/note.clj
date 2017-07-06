(ns reactor.handlers.note
  (:require [blueprints.models
             [account :as account]
             [note :as note]]
            [datomic.api :as d]
            [reactor.dispatch :as dispatch]
            [reactor.handlers.common :refer :all]
            [reactor.models.event :as event]
            [reactor.services.slack :as slack]
            [reactor.services.slack.message :as sm]))

;; =============================================================================
;; Create Note
;; =============================================================================

;; =============================================================================
;; Slack Notification


(defn- note-url [hostname note]
  (format "%s/admin/accounts/%s/notes" hostname (-> note note/account :db/id)))


(defmethod dispatch/report :note/create
  [deps event {:keys [uuid]}]
  (let [note (note/by-uuid (->db deps) uuid)
        type (if (note/ticket? note) "ticket" "note")]
    (slack/send
     (->slack deps)
     {:uuid    (event/uuid event)
      :channel slack/crm}
     (sm/msg
      (sm/info
       (sm/title (note/subject note)
                 (note-url (->public-hostname deps) note))
       (sm/text (note/content note))
       (sm/fields
        (sm/field "Account" (-> note note/account account/full-name) true)
        (when-let [author (note/author note)]
          (sm/field "Author" (account/full-name author) true))
        (sm/field "Type" type true)))))))


;; =============================================================================
;; Create


(defmethod dispatch/job :note/create
  [deps event {:keys [notify? subject content account-id] :as params}]
  (let [note (note/create subject content)
        tx   (if-let [account (d/entity (->db deps) account-id)]
               {:db/id account-id :account/notes note}
               note)]
    (if notify?
      [tx
       (event/report :note/create
                     {:params       {:uuid (:note/uuid note)}
                      :triggered-by event})]
      tx)))
