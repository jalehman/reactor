(ns reactor.utils.mail
  (:require [mailer.message :as mm]))


(def from-accounting
  "Accounting from address."
  "Starcity Accounting <accounting@starcity.com>")


(def accounting-sig
  "Accounting signature."
  (mm/sig "Starcity Accounting"))


(def community-address
  "The community@ address (bcc'd on all automated messages)"
  "community@starcity.com")


(def from-community
  "Community from address."
  "Starcity Community Team <community@starcity.com>")


(def community-sig
  "Community signature."
  (mm/sig "Starcity Community"))


(def from-operations
  "Operations from address."
  "Starcity Operations Team <ops@starcity.com>")


(def ops-sig
  "Ops signarture"
  (mm/sig "Starcity Operations"))

(def from-noreply
  "Default, noreply email address."
  "The Starcity Team <noreply@starcity.com>")

(def noreply-sig
  "The default, noreply signature"
  "The Starcity Team")


(defn subject
  "Subject line of an email."
  [s]
  (str "Starcity: " s))
