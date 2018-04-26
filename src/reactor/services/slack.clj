(ns reactor.services.slack
  (:refer-clojure :exclude [send])
  (:require [cheshire.core :as json]
            [clojure.core.async :refer [chan put!]]
            [clojure.string :as str]
            [org.httpkit.client :as http]
            [taoensso.timbre :as timbre]
            [toolbelt.core :as tb]))


;; =============================================================================
;; API
;; =============================================================================


(defn- ->channel [s]
  (if (or (str/starts-with? s "#") (str/starts-with? s "@"))
    s
    (str "#" s)))


(defn- send*
  [{:keys [webhook-url channel-override username]} {:keys [uuid channel]} msg]
  (let [out-c      (chan 1)
        msg-params {:username username
                    :channel  (->channel (or channel-override channel))}]
    (http/post webhook-url
               {:keepalive 30000
                :headers   {"Content-Type" "application/json"}
                :body      (json/generate-string (merge msg msg-params))}
               (fn [res]
                 (if-let [error (:error res)]
                   (put! out-c (ex-info "Error in Slack request!" (tb/assoc-when
                                                                   {:response error}
                                                                   :uuid uuid)))
                   (put! out-c res))))
    out-c))


(defprotocol ISlack
  "Interface to the Slack Webhook API."
  (send
    [this message]
    [this opts message]))


(defrecord Slack [webhook-url username channel-override])


(extend-protocol ISlack
  Slack
  (send
    ([this message]
     (send this {} message))
    ([this opts message]
     (send* this opts message))))


(defn slack
  [webhook-url username & [channel]]
  (map->Slack (tb/assoc-when
               {:webhook-url webhook-url
                :username username}
               :channel-override channel)))


;; =====================================
;; Channels


(def ops "#ops")
(def crm "#crm")
(def community "#community")
(def log "#webserver")
(def helping-hands "#helping-hands")
