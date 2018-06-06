(ns reactor.utils.mail
  (:require [mailer.message :as mm]))


(def from-accounting
  "Accounting from address."
  "Starcity Accounting <accounting@starcity.com>")


(def accounting-sig
  "Accounting signature."
  (mm/sig "Starcity Accounting"))


(def from-community
  "Community from address."
  "Starcity Community Team <community@starcity.com>")


(def community-sig
  "Community signature."
  (mm/sig "Starcity Community"))


(defn subject
  "Subject line of an email."
  [s]
  (str "Starcity: " s))
