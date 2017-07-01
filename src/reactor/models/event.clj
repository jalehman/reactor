(ns reactor.models.event
  (:refer-clojure :exclude [key])
  (:require [clojure.spec :as s]
            [datomic.api :as d]
            [toolbelt
             [core :as tb]
             [datomic :as td]
             [predicates :as p]]))

;; =============================================================================
;; Selectors
;; =============================================================================


(def key
  "The event's key."
  :event/key)

(s/fdef key
        :args (s/cat :event p/entity?)
        :ret keyword?)


(def uuid
  "The UUID of this event."
  :event/uuid)

(s/fdef uuid
        :args (s/cat :event p/entity?)
        :ret uuid?)


(defn params
  "Produce the event's parameters as EDN."
  [event]
  (when-some [ps (:event/params event)]
    (read-string ps)))

(s/fdef params
        :args (s/cat :event p/entity?)
        :ret (s/or :params map? :nothing nil?))


(def topic
  "The topic of this event."
  :event/topic)

(s/fdef topic
        :args (s/cat :event p/entity?)
        :ret (s/or :topic keyword? :nothing nil?))


(def triggered-by
  "The event that triggered this event."
  :event/triggered-by)

(s/fdef triggered-by
        :args (s/cat :event p/entity?)
        :ret (s/or :event p/entity? :nothing nil?))


(defn metadata
  "Produce the event's metadat as EDN."
  [event]
  (when-some [x (:event/meta event)]
    (read-string x)))

(s/fdef metadata
        :args (s/cat :event p/entity?)
        :ret (s/or :params map? :nothing nil?))


;; =============================================================================
;; Transactions
;; =============================================================================


(defn create
  [key {:keys [id uuid params meta topic triggered-by]
        :or   {uuid (d/squuid)}}]
  (let [params (when params (pr-str params))
        meta   (when meta (pr-str meta))]
    (tb/assoc-when
     {:db/id        (d/tempid :db.part/starcity)
      :event/uuid   uuid
      :event/key    key
      :event/status :event.status/pending}
     :event/id id
     :event/topic topic
     :event/triggered-by (td/id triggered-by)
     :event/params params
     :event/meta meta)))

(s/def ::id string?)
(s/def ::uuid uuid?)
(s/def ::params map?)
(s/def ::meta map?)
(s/def ::topic keyword?)
(s/def ::triggered-by p/entity?)

(s/fdef create
        :args (s/cat :key keyword?
                     :opts (s/keys :opt-un [::id ::uuid ::params ::meta ::topic ::triggered-by]))
        :ret map?)


(defn failed
  [event]
  {:db/id        (:db/id event)
   :event/status :event.status/failed})


(defn successful
  [event]
  {:db/id        (:db/id event)
   :event/status :event.status/successful})


;; =============================================================================
;; Queries
;; =============================================================================


(defn by-uuid
  "Look up an event by its `uuid`."
  [db uuid]
  (d/entity db [:event/uuid uuid]))

(defn by-id
  "Look up an event by its `id`."
  [db id]
  (d/entity db [:event/id id]))
