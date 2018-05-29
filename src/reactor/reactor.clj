(ns reactor.reactor
  (:require [clojure.core.async :as a]
            [clojure.spec.alpha :as s]
            [clj-time.core :as t]
            [clj-time.coerce :as c]
            [datomic.api :as d]
            [reactor.deps :as deps]
            [reactor.dispatch :as dispatch]
            [reactor.handlers.account]
            [reactor.handlers.application]
            [reactor.handlers.collaborator]
            [reactor.handlers.license-transition]
            [reactor.handlers.newsletter]
            [reactor.handlers.note]
            [reactor.handlers.order]
            [reactor.handlers.rent]
            [reactor.handlers.scheduler]
            [reactor.handlers.security-deposit]
            [reactor.handlers.session]
            [reactor.handlers.stripe]
            [blueprints.models.event :as event]
            [taoensso.timbre :as timbre]
            [toolbelt.async :as ta :refer [<!!?]]
            [toolbelt.core :as tb]
            [toolbelt.datomic :as td]
            [teller.core :as teller]))

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
        :args (s/cat :event td/entity?)
        :ret map?)


(defn- same-event? [e1 e2]
  (= (:db/id e1) (:db/id e2)))


(defn- gen-tx [dispatch deps event]
  (let [tx (dispatch deps event (event/params event))]
    (cond
      (and (sequential? tx)
           (some (partial same-event? event) tx))  tx
      (sequential? tx)                       (conj tx (event/successful event))
      (and (map? tx) (same-event? event tx)) [tx]
      (map? tx)                              [tx (event/successful event)]
      (ta/chan? tx)                          (do (<!!? tx) [(event/successful event)])
      :otherwise                             [(event/successful event)])))


(defn- process-events
  [conn deps db events]
  (doseq [event events]
    (try
      (let [deps (assoc deps :db db)]
        (timbre/info (event/key event) (event->map event))
        (let [tx (gen-tx (dispatch/dispatch (event/topic event)) deps event)]
          @(d/transact-async conn tx)))
      (catch Throwable t
        (timbre/error t (event/key event) (event->map event))
        (let [err-tx (get (ex-data t) :tx)]
          @(d/transact-async conn (concat [(event/failed event)] err-tx)))))))


(defn- start-queue!
  "Construct a new queue on `mult`. All transaction reports received are passed
  through `extraction-fn`, which is expected to produce entity ids. Constructed
  entities are then passed to `dispatch-fn` (along with the transaction report)
  for processing."
  [conn mult deps extraction-fn & {:keys [buf-size]
                                   :or   {buf-size 4096}}]
  (let [c (a/chan (a/sliding-buffer buf-size))]
    (a/go-loop []
      (when-let [txr (a/<! c)]
        (try
          (when-let [events (extraction-fn txr)]
            (process-events conn deps (:db-after txr) events))
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


;; =============================================================================
;; API
;; =============================================================================


;; =============================================================================
;; Pending Events


(defn fetch-pending-events [db since]
  (->> (d/q '[:find [?e ...]
              :in $ ?since
              :where
              [?e :event/status :event.status/pending ?tx]
              [?tx :db/txInstant ?tx-time]
              [(.after ^java.util.Date ?tx-time ?since)]]
            db since)
       (map (partial d/entity db))))


(defn process-pending-events!
  "Process all events that are `:event.status/pending` that have acquired that
  status in the last two days."
  [conn deps]
  (let [since  (c/to-date (t/minus (t/now) (t/days 2)))
        events (fetch-pending-events (d/db conn) since)]
    (timbre/info ::process-pending {:count (count events)})
    (process-events conn deps (d/db conn) events)))


;; =============================================================================
;; Lifecycle


;; `job`: events that perform arbitrary work; default when no other topic is specified.
;; `:notify`: events that involve notifying members (external)
;; `:report`: events that involve reporting information to us (internal)
;; `:stripe`: stripe-specific events (webhooks)


(def ^:private topics
  [:notify :report :stripe :job])


(defn- start-queues!
  [conn mult deps]
  (reduce
   (fn [acc topic]
     (timbre/info ::start {:topic topic})
     (let [q (start-queue! conn mult deps (partial extract-pending topic) :topic topic)]
       (conj acc [topic q])))
   []
   topics))


(defn- install-report-queue
  "On a separate thread, take values from the `tx-report-queue` over `conn` and
  put them onto channel `c`. This is essantially just a `core.async` wrapper
  around the `tx-report-queue`."
  [conn c]
  (a/thread
    (try
      (let [queue (d/tx-report-queue conn)]
        (while true
          (let [report (.take queue)]
            (a/>!! c report))))
      (catch Exception e
        (timbre/error e "TX-REPORT-TAKE exception")
        (throw e)))))


(defn start!
  "Start a queue for each topic in `topics`."
  ([conn teller tx-report-ch]
   (start! conn tx-report-ch (deps/deps teller)))
  ([conn teller tx-report-ch conf]
   (let [mult            (a/mult tx-report-ch)
         tx-report-queue (install-report-queue conn tx-report-ch)
         deps            (deps/deps teller conf)
         queues          (start-queues! conn mult deps)]
     (process-pending-events! conn deps)
     {:conn            conn
      :tx-report-ch    tx-report-ch
      :tx-report-queue tx-report-queue
      :mult            mult
      :queues          queues})))

(s/fdef start!
        :args (s/cat :conn td/conn?
                     :teller teller/connection?
                     :chan ta/chan?
                     :conf (s/? deps/config?)))


(defn stop!
  "Clean up all reactor-related resources."
  [{:keys [conn tx-report-ch mult queues] :as reactor}]
  (a/close! tx-report-ch)
  (d/remove-tx-report-queue conn)
  (doseq [[t q] queues]
    (timbre/info ::stop {:topic t})
    (stop-queue! mult q)))
