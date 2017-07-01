(ns reactor.reactor
  (:require [clojure.core.async :as a]
            [clojure.spec :as s]
            [datomic.api :as d]
            [reactor.dispatch :as dispatch]
            [reactor.handlers.account]
            [reactor.handlers.application]
            [reactor.handlers.collaborator]
            [reactor.handlers.newsletter]
            [reactor.handlers.note]
            [reactor.models.event :as event]
            [taoensso.timbre :as timbre]
            [toolbelt
             [async :refer [<!!?]]
             [core :as tb]
             [predicates :as p]]
            [ribbon.core :as ribbon]))

;; =============================================================================
;; Internal
;; =============================================================================


(defn- event->map [event]
  (let [m (-> (into {} event)
              (assoc :db/id (:db/id event))
              (update :event/triggered-by :event/uuid)
              (merge (when-some [x (:event/params event)]
                       {:event/params (read-string x)})))]
    (reduce
     (fn [acc [k v]]
       (tb/assoc-some acc k v))
     {}
     m)))

(s/fdef event->map
        :args (s/cat :event p/entity?)
        :ret map?)


(defn- gen-tx [dispatch deps event]
  (let [tx (dispatch deps event (event/params event))]
    (cond
      (and (sequential? tx)
           (some #(contains? % :event/status) tx))
      tx

      (sequential? tx)
      (conj tx (event/successful event))

      (and (map? tx) (contains? tx :event/status))
      [tx]

      (map? tx)
      [tx (event/successful event)]

      (p/chan? tx)
      (let [_ (<!!? tx)]
        [(event/successful event)])

      :otherwise
      [(event/successful event)])))


(defn- start-queue!
  "Construct a new queue on `mult`. All transaction reports received are passed
  through `extraction-fn`, which is expected to produce entity ids. Constructed
  entities are then passed to `dispatch-fn` (along with the transaction report)
  for processing."
  [conn mult deps extraction-fn & {:keys [topic buf-size]
                                   :or   {topic    ::topicless
                                          buf-size 4096}}]
  (let [c (a/chan (a/sliding-buffer buf-size))]
    (a/go-loop []
      (when-let [txr (a/<! c)]
        (try
          (when-let [events (extraction-fn txr)]
            (doseq [event events]
              (try
                (let [deps (assoc deps :db (:db-after txr))]
                  (timbre/info (:event/key event) (event->map event))
                  @(d/transact-async conn (gen-tx (dispatch/dispatch topic) deps event)))
                (catch Throwable t
                  (timbre/error t (:event/key event) (event->map event))
                  @(d/transact-async conn [(event/failed event)])))))
          (catch Throwable t
            (timbre/error t "extraction error!")))
        (recur)))
    (a/tap mult c)
    c))


(defn- stop-queue!
  "Clean up queue resources."
  [mult queue]
  (a/untap mult queue)
  (a/close! queue))


(defn- pending-datoms
  [txr]
  (let [cmd-status-attr-id (d/entid (:db-after txr) :event/status)
        pending-status-id  (d/entid (:db-after txr) :event.status/pending)]
    (seq (reduce (fn [acc [e a v _ add :as datom]]
                   (if (and (= a cmd-status-attr-id)
                            (= v pending-status-id)
                            add)
                     (conj acc e)
                     acc))
                 []
                 (:tx-data txr)))))


(defn- extract-pending
  ([txr]
   (extract-pending nil txr))
  ([topic txr]
   (when-some [ds (pending-datoms txr)]
     (->> (map (partial d/entity (:db-after txr)) ds)
          (filter (comp #{topic} :event/topic))))))


(defn- extract-topicless
  [txr topics]
  (when-some [ds (pending-datoms txr)]
    (->> (map (partial d/entity (:db-after txr)) ds)
         (filter #(or (nil? (:event/topic %))
                      (not ((set topics) (:event/topic %))))))))


(defn- start-topicless-queue!
  [conn mult deps topics]
  (let [q (start-queue! conn mult deps #(extract-topicless % topics))]
    (timbre/info ::start {:topic ::topicless})
    [::topicless q]))


;; =============================================================================
;; API
;; =============================================================================


(s/def ::mailer #(satisfies? mailer.core/Mailer %))
(s/def ::community-safety #(satisfies? reactor.services.community-safety/ICommunitySafety %))
(s/def ::slack #(satisfies? reactor.services.slack/ISlack %))
(s/def ::weebly #(satisfies? reactor.services.weebly/WeeblyPromote %))
(s/def ::stripe ribbon/conn?)
(s/def ::public-hostname string?)
(s/def ::deps
  (s/keys :req-un [::mailer
                   ::community-safety
                   ::slack
                   ::weebly
                   ::stripe
                   ::public-hostname]))


;; =============================================================================
;; Lifecycle


(def ^:private topics
  [:mail :slack :stripe])


(defn start!
  "Start a queue for each topic in `topics`."
  [conn mult deps]
  (reduce
   (fn [acc topic]
     (timbre/info ::start {:topic topic})
     (let [q (start-queue! conn mult deps (partial extract-pending topic) :topic topic)]
       (conj acc [topic q])))
   [(start-topicless-queue! conn mult deps topics)]
   topics))

(s/fdef start!
        :args (s/cat :conn p/conn?
                     :mult any?
                     :deps ::deps))


(defn stop!
  "Shut down all `queues`."
  [mult queues]
  (doseq [[t q] queues]
    (timbre/info ::stop {:topic t})
    (stop-queue! mult q)))


;; =============================================================================
;; Dependencies


(defn deps
  "Construct the dependencies map for Reactor to function."
  [community-safety mailer slack weebly stripe public-hostname]
  {:community-safety community-safety
   :mailer           mailer
   :slack            slack
   :weebly           weebly
   :stripe           stripe
   :public-hostname  public-hostname})

(s/fdef deps
        :args (s/cat :community-safety ::community-safety
                     :mailer ::mailer
                     :slack ::slack
                     :weebly ::weebly
                     :stripe ::stripe
                     :public-hostname ::public-hostname)
        :ret ::deps)
