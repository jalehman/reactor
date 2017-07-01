(ns reactor.config
  (:require [aero.core :as aero]
            [mount.core :as mount :refer [defstate]]
            [clojure.java.io :as io]
            [toolbelt.core :as tb]))


(defstate config
  :start (-> (io/resource "config.edn")
             (aero/read-config {:resolver aero/root-resolver
                                :profile  (:env (mount/args))})))


;; =============================================================================
;; Hosts
;; =============================================================================


(defn public-hostname
  "The hostname of this public server."
  [config]
  (get-in config [:hosts :public]))


;; =============================================================================
;; Datomic
;; =============================================================================


(defn datomic
  "The Datomic configuration. Contains `:uri` and `:partition`"
  [config]
  (:datomic config))


(defn datomic-partition
  "The Datomic partition."
  [config]
  (:partition (datomic config)))


(defn tx-report-buffer-size
  "The size of the Datomic tx-report queue."
  [config]
  (tb/str->int (get-in config [:datomic :tx-report-buffer-size])))


;; =============================================================================
;; nrepl
;; =============================================================================


(defn nrepl-port
  "Port to run the nrepl server on."
  [config]
  (tb/str->int (get-in config [:nrepl :port])))


;; =============================================================================
;; Stripe
;; =============================================================================


(defn stripe-public-key
  "The Stripe public key."
  [config]
  (get-in config [:secrets :stripe :public-key]))


(defn stripe-secret-key
  "The Stripe secret key."
  [config]
  (get-in config [:secrets :stripe :secret-key]))


;; =============================================================================
;; Mailgun
;; =============================================================================


(defn mailgun-domain
  [config]
  (get-in config [:mailgun :domain]))


(defn mailgun-sender
  [config]
  (get-in config [:mailgun :sender]))


(defn mailgun-api-key
  [config]
  (get-in config [:mailgun :api-key]))


;; =============================================================================
;; Community Safety
;; =============================================================================


(defn community-safety-api-key
  [config]
  (get-in config [:secrets :community-safety :api-key]))


;; =============================================================================
;; Weebly
;; =============================================================================

(defn weebly-site-id
  [config]
  (get-in config [:secrets :weebly :site-id]))


(defn weebly-form-id
  [config]
  (get-in config [:secrets :weebly :form-id]))


;; =============================================================================
;; Slack
;; =============================================================================

(defn slack-client-id
  [config]
  (get-in config [:secrets :slack :client-id]))


(defn slack-secret-key
  [config]
  (get-in config [:secrets :slack :client-secret]))


(defn slack-webhook-url
  [config]
  (get-in config [:secrets :slack :webhook]))


(defn slack-username
  [config]
  (get-in config [:slack :username]))


;; =============================================================================
;; environments
;; =============================================================================


(defn development? [config]
  (= :dev (:env (mount/args))))


(defn staging? [config]
  (= :stage (:env (mount/args))))


(defn production? [config]
  (= :prod (:env (mount/args))))


;; =============================================================================
;; log
;; =============================================================================


(defn log-level
  [config]
  (get-in config [:log :level]))


(defn log-appender
  [config]
  (get-in config [:log :appender]))


(defn log-file
  [config]
  (get-in config [:log :file]))
