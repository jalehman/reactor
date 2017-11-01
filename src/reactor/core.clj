(ns reactor.core
  (:gen-class)
  (:require [blueprints.core :as blueprints]
            [cheshire.core :as json]
            [clojure.core.async :as a]
            [clojure.spec :as s]
            [clojure.string :as string]
            [clojure.tools.cli :as cli]
            [clojure.tools.nrepl.server :as nrepl]
            [datomic.api :as d]
            [drawknife.core :as drawknife]
            [mock.mock :as mock]
            [mount.core :as mount :refer [defstate]]
            [reactor.config :as config :refer [config]]
            [reactor.deps :as deps]
            [reactor.reactor :as reactor]
            [taoensso.timbre :as timbre]
            [taoensso.timbre.appenders.3rd-party.rolling :as rolling]
            [taoensso.timbre.appenders.core :as appenders]))

;; =============================================================================
;; State
;; =============================================================================


;; =============================================================================
;; Datomic


(defn- new-connection [{:keys [uri partition] :as conf}]
  (timbre/info :datomic/connecting {:uri (string/replace uri #"password.*" "")})
  (d/create-database uri)
  (let [conn (d/connect uri)]
    (blueprints/conform-db conn partition)
    conn))


(defn- disconnect [conn]
  (timbre/info :datomic/disconnecting)
  (d/release conn))


(defstate conn
  :start (new-connection (config/datomic config/config))
  :stop  (disconnect conn))


;; =============================================================================
;; Logging


(defstate logger
  :start (timbre/merge-config!
          (drawknife/configuration (config/log-level config)
                                   (config/log-appender config)
                                   (config/log-file config))))


;; =============================================================================
;; nrepl


(defn- start-nrepl [port]
  (timbre/info :nrepl/start {:port port})
  (nrepl/start-server :port port))


(defstate nrepl
  :start (start-nrepl (config/nrepl-port config))
  :stop  (nrepl/stop-server nrepl))


;; =============================================================================
;; reactor


(defn- prod-config [config]
  {:mailer           {:api-key (config/mailgun-api-key config)
                      :domain  (config/mailgun-domain config)}
   :slack            {:webhook-url (config/slack-webhook-url config)
                      :username    (config/slack-username config)}
   :community-safety {:api-key (config/community-safety-api-key config)}
   :weebly           {:site-id (config/weebly-site-id config)
                      :form-id (config/weebly-form-id config)}
   :stripe           {:secret-key (config/stripe-secret-key config)}
   :public-hostname  (config/public-hostname config)})


(defn- dev-config [config]
  {:mailer          {:api-key (config/mailgun-api-key config)
                     :domain  (config/mailgun-domain config)
                     :sender  (config/mailgun-sender config)
                     :send-to "josh@joinstarcity.com"}
   :slack           {:webhook-url (config/slack-webhook-url config)
                     :username    (config/slack-username config)
                     :channel     "#debug"}
   :stripe          {:secret-key (config/stripe-secret-key config)}
   :public-hostname (config/public-hostname config)})


(defstate reactor
  :start (let [conf (if (config/production? config)
                      (prod-config config)
                      (dev-config config))
               chan (a/chan (a/sliding-buffer (config/tx-report-buffer-size config)))]
           (reactor/start! conn chan conf))
  :stop (reactor/stop! reactor))


;; =============================================================================
;; Main
;; =============================================================================


(def cli-options
  [["-e" "--environment ENVIRONMENT" "The environment to start the server in."
    :id :env
    :default :production
    :parse-fn keyword
    :validate [#{:prod :dev :stage} "Must be one of #{prod dev stage"]]])


(defn- exit [status msg]
  (System/exit status))


(defn -main [& args]
  (let [{:keys [options errors]} (cli/parse-opts args cli-options)]
    (when errors
      (exit 1 (string/join "\n" errors)))
    (mount/start-with-args {:env (:env options)})))
