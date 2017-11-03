(ns reactor.handlers.common
  (:require [blueprints.models.account :as account]
            [blueprints.models.event :as event]
            [reactor.services.slack.message :as sm]))


;; =============================================================================
;; Dependencies
;; =============================================================================


(defn ->db [deps] (:db deps))


(defn ->public-hostname [deps] (:public-hostname deps))


(defn ->mailer [deps] (:mailer deps))


(defn ->cf [deps] (:community-safety deps))


(defn ->slack [deps] (:slack deps))


(defn ->weebly [deps] (:weebly deps))


(defn ->stripe [deps] (:stripe deps))


;; =============================================================================
;; Report Helpers
;; =============================================================================


(defn account-link [hostname account]
  (let [url (format "%s/admin/accounts/%s" hostname (:db/id account))]
    (sm/link url (account/full-name account))))
