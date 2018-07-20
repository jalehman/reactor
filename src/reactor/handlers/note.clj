(ns reactor.handlers.note
  (:require [blueprints.models.account :as account]
            [blueprints.models.event :as event]
            [blueprints.models.events :as events]
            [blueprints.models.member-license :as member-license]
            [blueprints.models.note :as note]
            [blueprints.models.property :as property]
            [clojure.string :as string]
            [datomic.api :as d]
            [reactor.dispatch :as dispatch]
            [reactor.handlers.common :refer :all]
            [reactor.services.slack :as slack]
            [reactor.services.slack.message :as sm]
            [toolbelt.core :as tb]
            [taoensso.timbre :as timbre]))

;; =============================================================================
;; Helpers
;; =============================================================================


(defn- note-url [hostname note]
  (format "%s/accounts/%s/notes" hostname (-> note note/account :db/id)))


(defn- scrub-text [s]
  (-> (string/replace s #"&#39;" "'")
      (string/replace #"&quot;" "\"")))


;; =============================================================================
;; Create Note
;; =============================================================================


(defn- property-codes [db refs]
  (set (map
        #(cond
           (some? (account/email %)) (property/code (account/current-property db %))
           (some? (property/code %)) (property/code %)
           :otherwise nil)
        refs)))


(defn- property-channel [property]
  (or (property/slack-channel property) slack/crm))


(defn- notification-channel [db note]
  (let [property (when-let [a (note/account note)]
                   (account/current-property db a))]
    (property-channel property)))


(defn- notification-channels [db refs]
  (->> (property-codes db refs)
       (map (comp property-channel (partial d/entity db) (partial conj [:property/code])))))


(defn- mentions [refs]
  (->> refs
       (map
        #(cond
           (some? (account/email %)) (account/short-name %)
           (some? (property/code %)) (property/code %)
           :otherwise                nil))
       (remove nil?)))


(defmethod dispatch/report :note/created
  [deps event {:keys [uuid slack-channel] :as params}]
  (let [note     (note/by-uuid (->db deps) uuid)
        type     (if (note/ticket? note) "ticket" "note")
        mentions (apply str (interpose ", " (-> note note/refs mentions)))]
    (slack/send
     (->slack deps)
     {:uuid    (event/uuid event)
      :channel slack-channel}
     (sm/msg
      (sm/info
       (sm/title (scrub-text (note/subject note))
                 (note-url (->dashboard-hostname deps) note))
       (sm/text (scrub-text (note/content note)))
       (sm/fields
        (sm/field "Mentions" mentions true)
        (when-let [author (note/author note)]
          (sm/field "Author" (account/short-name author) true))))))))


(defmethod dispatch/job :note/created [deps event {:keys [refs uuid] :as params}]
  (let [note     (note/by-uuid (->db deps) uuid)
        channels (notification-channels (->db deps) (note/refs note))]
    (map
     #(event/report (event/key event) {:params       (assoc params :slack-channel %)
                                       :triggered-by event})
     channels)))


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
     {:channel (notification-channel (->db deps) parent)
      :uuid    (event/uuid event)}
     (sm/msg
      (sm/info
       (sm/title (format "%s commented on a note." (-> note note/author account/short-name))
                 (note-url (->dashboard-hostname deps) (note/parent note)))
       (sm/text (format "_%s_" (note/content note)))
       (sm/fields
        (sm/field "Parent" (note/subject parent) true)
        (sm/field "Account" (-> parent note/account account/short-name) true)))))))


(defmethod dispatch/job :note.comment/created [deps event params]
  (event/report (event/key event) {:params       params
                                   :triggered-by event}))
