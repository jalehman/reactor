(ns reactor.handlers.note
  (:require [blueprints.models
             [account :as account]
             [event :as event]
             [events :as events]
             [note :as note]]
            [datomic.api :as d]
            [reactor.dispatch :as dispatch]
            [reactor.handlers.common :refer :all]
            [reactor.services.slack :as slack]
            [reactor.services.slack.message :as sm]
            [toolbelt.core :as tb]))


;; =============================================================================
;; Helpers
;; =============================================================================


(defn- note-url [hostname note]
  (format "%s/admin/accounts/%s/notes" hostname (-> note note/account :db/id)))


;; =============================================================================
;; Create Note
;; =============================================================================


(defmethod dispatch/report :note/created
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


(defmethod dispatch/job :note/created [deps event params]
  (event/report (event/key event) {:params       params
                                   :triggered-by event}))


(defmethod dispatch/job :note/create
  [deps event {:keys [notify? subject content account-id] :as params}]
  (let [note (note/create subject content)
        tx   (if-let [account (d/entity (->db deps) account-id)]
               {:db/id account-id :account/notes note}
               note)]
    (tb/conj-when [tx] (when notify? (events/note-created note)))))


;; =============================================================================
;; Comment Added
;; =============================================================================


(defmethod dispatch/report :note.comment/created [deps event {uuid :comment-uuid}]
  (let [note   (note/by-uuid (->db deps) uuid)
        parent (note/parent note)]
    (slack/send
     (->slack deps)
     {:channel slack/crm
      :uuid    (event/uuid event)}
     (sm/msg
      (sm/info
       (sm/title (format "%s commented on a note." (-> note note/author account/full-name))
                 (note-url (->public-hostname deps) (note/parent note)))
       (sm/text (format "_%s_" (note/content note)))
       (sm/fields
        (sm/field "Parent" (note/subject parent))
        (sm/field "Account" (-> parent note/account account/full-name))))))))


(defmethod dispatch/job :note.comment/created [deps event params]
  (event/report (event/key event) {:params       params
                                   :triggered-by event}))
