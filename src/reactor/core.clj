(ns reactor.core
  (:gen-class)
  (:require [blueprints.core :as blueprints]
            [cheshire.core :as json]
            [clojure
             [spec :as s]
             [string :as string]]
            [clojure.core.async :as a]
            [clojure.tools.nrepl.server :as nrepl]
            [datomic.api :as d]
            [mailer.core :as mailer]
            [mock.mock :as mock]
            [mount.core :as mount :refer [defstate]]
            [reactor
             [config :as config :refer [config]]
             [deps :as deps]
             [reactor :as reactor]]
            [reactor.services
             [community-safety :as cs]
             [slack :as slack]
             [weebly :as weebly]]
            [ribbon.core :as ribbon]
            [taoensso.timbre :as timbre]
            [taoensso.timbre.appenders.3rd-party.rolling :as rolling]
            [taoensso.timbre.appenders.core :as appenders]))

;; =============================================================================
;; State
;; =============================================================================

;; =============================================================================
;; Community Safety


(defstate community-safety
  :start (if (config/production? config)
           (cs/community-safety (config/community-safety-api-key config))
           (mock/community-safety)))


;; =============================================================================
;; Datomic


(defn- new-connection [{:keys [uri partition] :as conf}]
  (timbre/info ::connecting {:uri (string/replace uri #"password.*" "")})
  (d/create-database uri)
  (let [conn (d/connect uri)]
    (blueprints/conform-db conn partition)
    conn))


(defn- disconnect [conn]
  (timbre/info ::disconnecting)
  (d/release conn))


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


(defstate conn
  :start (new-connection (config/datomic config/config))
  :stop  (disconnect conn))


(defstate ^:private tx-report-ch
  :start (a/chan (a/sliding-buffer (config/tx-report-buffer-size config)))
  :stop (a/close! tx-report-ch))


(defstate ^:private tx-report-queue
  :start (install-report-queue conn tx-report-ch)
  :stop (d/remove-tx-report-queue conn))


(defstate mult :start (a/mult tx-report-ch))


;; =============================================================================
;; Logging


(defn- appender [appender filename]
  (case appender
    :spit    {:spit (appenders/spit-appender {:fname filename})}
    :rolling {:rolling (rolling/rolling-appender {:path filename})}))


(s/def ::event-vargs
  (s/cat :event keyword?
         :params map?))


(defn- event-vargs
  [data event params]
  (try
    (assoc data :vargs
           [(-> {:event event}
                (merge (when-let [err (:?err data)] {:error-data (or (ex-data err) :none)})
                       params)
                json/generate-string)])
    (catch Throwable t
      (timbre/warn t "Error encountered while attempting to encode vargs.")
      data)))


(defn- wrap-event-format
  "Middleware that transforms the user's log input into a JSON
  string with an `event` key. This is used to make search effective in LogDNA.

  Only applies when timbre is called with input of the form:

  (timbre/info ::event {:map :of-data})"
  [{:keys [vargs] :as data}]
  (if (s/valid? ::event-vargs vargs)
    (let [{:keys [event params]} (s/conform ::event-vargs vargs)]
      (event-vargs data event params))
    data))


(defstate logger
  :start (timbre/merge-config!
          {:level      (config/log-level config)
           :middleware [wrap-event-format]
           :appenders  (appender (config/log-appender config)
                                 (config/log-file config))}))



;; =============================================================================
;; mailer


(defstate mailer
  :start (if (config/production? config)
           (mailer/mailgun (config/mailgun-api-key config)
                           (config/mailgun-domain config))
           (mailer/mailgun (config/mailgun-api-key config)
                           (config/mailgun-domain config)
                           {:sender  (config/mailgun-sender config)
                            :send-to "josh@joinstarcity.com"})))


;; =============================================================================
;; nrepl


(defn- start-nrepl [port]
  (timbre/info ::starting {:port port})
  (nrepl/start-server :port port))


(defstate nrepl
  :start (start-nrepl (config/nrepl-port config))
  :stop  (nrepl/stop-server nrepl))


;; =============================================================================
;; Slack


(defstate slack
  :start (if (config/production? config)
           (slack/slack (config/slack-webhook-url config)
                        (config/slack-username config))
           (slack/slack (config/slack-webhook-url config)
                        (config/slack-username config)
                        "#debug")))


;; =============================================================================
;; Weebly


(defstate weebly
  :start (if (config/production? config)
           (weebly/weebly (config/weebly-site-id config)
                          (config/weebly-form-id config))
           (mock/weebly)))


;; =============================================================================
;; Stripe


(defstate stripe
  :start (if (config/production? config)
           (ribbon/stripe-connection (config/stripe-secret-key config))
           (mock/stripe)))


;; =============================================================================
;; reactor


(defstate reactor
  :start (let [deps (deps/deps community-safety
                               mailer
                               slack
                               weebly
                               stripe
                               (config/public-hostname config))]
           (reactor/start! conn mult deps))
  :stop (reactor/stop! mult reactor))
