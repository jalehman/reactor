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
;; topicless

(defmulti topicless
  "The dispatch function for events to be processed on the `topicless` queue."
  key-dispatch)


(defmethod topicless :default [_ event _]
  (default-dispatch :topicless/unhandled event))


(defmethod dispatch :default [topic] topicless)


;; =============================================================================
;; mail


(defmulti mail
  "The dispatch function for events to be processed on the `mail` queue."
  key-dispatch)


(defmethod mail :default [_ event _]
  (default-dispatch :mail/unhandled event))


(defmethod dispatch :mail [_] mail)


;; =============================================================================
;; Slack


(defmulti slack
  "The dispatch function for events to be processed on the `slack` queue."
  key-dispatch)


(defmethod slack :default [_ event _]
  (default-dispatch :slack/unhandled event))


(defmethod dispatch :slack [_] slack)


;; =============================================================================
;; Stripe


(defmulti stripe
  "The dispatch function for events to be processed on the `stripe` queue."
  key-dispatch)


(defmethod stripe :default [_ event _]
  (default-dispatch :stripe/unhandled event))


(defmethod dispatch :stripe [_] stripe)
