(ns mock.mock
  (:require [clj-time.coerce :as c]
            [clj-time.core :as t]
            [clojure.core.async :as a]
            [datomic.api :as d]
            [mailer.core :as mailer]
            [reactor.services.community-safety :as cs]
            [reactor.services.slack :as slack]
            [reactor.services.weebly :as weebly]
            [ribbon.core :as ribbon]
            [toolbelt.core :as tb]))

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- tempid [ref]
  (if (some? ref)
    (d/tempid :db.part/starcity ref)
    (d/tempid :db.part/starcity)))


;; =============================================================================
;; Transaction Data
;; =============================================================================


(defn account-tx
  [& {:keys [first-name last-name email license role ref]
      :or   {first-name "Jocelyn"
             last-name  "Robancho"
             role       :account.role/applicant
             email      "test@test.com"}}]
  (tb/assoc-when
   {:db/id              (tempid ref)
    :account/first-name first-name
    :account/last-name  last-name
    :account/role       role
    :account/email      email}
   :account/licenses license))


(defn member-license-tx
  [& {:keys [status unit price commencement sub-id ref]
      :or   {status       :member-license.status/active
             unit         [:unit/name "52gilbert-1"]
             price        2100.0
             commencement (c/to-date (t/date-time 2017 1 1))}}]
  (tb/assoc-when
   {:db/id                       (tempid ref)
    :member-license/status       status
    :member-license/unit         unit
    :member-license/rate        price
    :member-license/commencement commencement}
   :member-license/subscription-id sub-id))


;; =============================================================================
;; Services
;; =============================================================================


(defn community-safety []
  (reify cs/ICommunitySafety
    (background-check [this user-id first-name last-name email dob]
      (cs/background-check this user-id first-name last-name email dob {}))
    (background-check [this user-id first-name last-name email dob opts]
      (let [c (a/chan 1)]
        (a/put! c {:body {} :headers {:location "https://test-community-safety.joinstarcity.com"}})
        c))))


(defn mailer []
  (reify mailer/Mailer
    (send [this to subject body]
      (mailer/send this to subject body {}))
    (send [this to subject body opts]
      (let [c (a/chan 1)]
        (a/put! c {:subject subject
                   :to      to
                   :body    body})
        c))))


(defn slack []
  (reify slack/ISlack
    (send [this message]
      (slack/send this message {}))
    (send [this opts message]
      (let [c (a/chan 1)]
        (a/put! c message)
        c))))


(defn stripe [& [payload]]
  (reify ribbon/RibbonRequest
    (request [this conf]
      (ribbon/request this conf {}))
    (request [this conf params]
      (let [c (a/chan 1)]
        (a/put! c (or payload {:conf conf :params params}))
        c))))


(defn weebly []
  (reify weebly/WeeblyPromote
    (subscribe! [this email]
      (let [c (a/chan 1)]
        (a/put! c {:body {:email email}})
        c))))
