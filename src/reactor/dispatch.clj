(ns reactor.dispatch
  (:require [taoensso.timbre :as timbre]
            [datomic.api :as d]))


;; =============================================================================
;; Helpers
;; =============================================================================


(defn- key-dispatch [db event params]
  (:event/key event))


(defn- event->map [event]
  (-> (into {} event)
      (assoc :db/id (:db/id event))))


(defn- default-dispatch [log-key event]
  (timbre/trace log-key (event->map event))
  {:db/id        (:db/id event)
   :event/status :event.status/seen})


;; =============================================================================
;; Dispatch
;; =============================================================================


(defmulti dispatch
  "Produce the dispatch function for `topic`."
  (fn [topic] topic))


;; =============================================================================
;; job

(defmulti job
  "The dispatch function for events to be processed on the `job` queue."
  key-dispatch)


(defmethod job :default [_ event _]
  (default-dispatch :job/unhandled event))


(defmethod dispatch :default [topic] job)


(defmethod dispatch :job [topic] job)


;; =============================================================================
;; notify


(defmulti notify
  "The dispatch function for events to be processed on the `notify` queue."
  key-dispatch)


(defmethod notify :default [_ event _]
  (default-dispatch :notify/unhandled event))


(defmethod dispatch :notify [_] notify)


;; =============================================================================
;; Report


(defmulti report
  "The dispatch function for events to be processed on the `report` queue."
  key-dispatch)


(defmethod report :default [_ event _]
  (default-dispatch :report/unhandled event))


(defmethod dispatch :report [_] report)


;; =============================================================================
;; Stripe


(defmulti stripe
  "The dispatch function for events to be processed on the `stripe` queue."
  key-dispatch)


(defmethod stripe :default [_ event _]
  (default-dispatch :stripe/unhandled event))


(defmethod dispatch :stripe [_] stripe)
