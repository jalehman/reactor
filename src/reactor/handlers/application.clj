(ns reactor.handlers.application
  (:require [blueprints.models.account :as account]
            [blueprints.models.address :as address]
            [blueprints.models.application :as application]
            [blueprints.models.event :as event]
            [clojure.string :as string]
            [datomic.api :as d]
            [hiccup.core :as html]
            [mailer.core :as mailer]
            [mailer.message :as mm]
            [reactor.dispatch :as dispatch]
            [reactor.handlers.common :refer :all]
            [reactor.services.community-safety :as cf]
            [reactor.services.slack :as slack]
            [reactor.services.slack.message :as sm]
            [reactor.utils.mail :as mail]
            [toolbelt.async :refer [<!!?]]
            [toolbelt.core :as tb]))

;; =============================================================================
;; Application Submission
;; =============================================================================


(defn- rand-doge []
  (let [phrases ["Such marketing" "Wow" "Much victory"
                 "Great success" "Very amazing"
                 "Dope" "So skilled"]]
    (->> phrases count rand-int (get phrases))))


(defmethod dispatch/report :application/submit
  [deps event {app-id :application-id}]
  (let [account (-> (d/entity (->db deps) app-id) application/account)
        title   (format "%s's application" (account/full-name account))
        link    (format "%s/accounts/%s"
                        (->dashboard-hostname deps)
                        (:db/id account))]
    (slack/send
     (->slack deps)
     {:uuid    (:event/uuid event)
      :channel slack/crm}
     (sm/msg
      (sm/success
       (sm/title title link)
       (sm/text (format "%s! Someone signed up! :partyparrot:" (rand-doge)))
       (sm/fields
        (sm/field "Email" (account/email account) true)
        (sm/field "Phone" (account/phone-number account) true)))))))


(defmethod dispatch/notify :application/submit
  [deps event {app-id :application-id}]
  (let [app     (d/entity (->db deps) app-id)
        account (application/account app)]
    (mailer/send
     (->mailer deps)
     (account/email account)
     (mail/subject "We are Reviewing Your Application")
     (mm/msg
      (mm/greet (account/first-name account))
      (mm/p "Thank you for completing Starcity's membership application. Next:")
      (html/html
       [:ol
        [:li "We'll process your application (community safety and financial checks) to pre-qualify you for the community,"]
        [:li "and then notify you as soon as you're pre-qualified."]])
      (mm/p "Stay tuned and thanks for your patience!")
      mail/community-sig)
     {:from mail/from-community
      :uuid (event/uuid event)})))


(defn- community-safety-check!
  [cf account app]
  (let [address     (application/address app)
        middle-name (account/middle-name account)]
    (<!!? (cf/background-check cf
                               (:db/id account)
                               (account/first-name account)
                               (account/last-name account)
                               (account/email account)
                               (account/dob account)
                               (tb/assoc-when
                                {:address {:city        (address/city address)
                                           :state       (address/state address)
                                           :postal-code (address/postal-code address)}}
                                :middle-name (if (string/blank? middle-name) nil middle-name))))))


(defmethod dispatch/job :application.submit/community-safety-check
  [deps event {app-id :application-id}]
  (let [app     (d/entity (->db deps) app-id)
        account (application/account app)
        res     (community-safety-check! (->cf deps) account app)]
    {:db/id                       (d/tempid :db.part/starcity)
     :community-safety/account    (:db/id account)
     :community-safety/report-url (cf/report-url res)}))


(defmethod dispatch/job :application/submit
  [_ event params]
  [(event/job :application.submit/community-safety-check
              {:params params :triggered-by event})

   (event/notify :application/submit {:params params :triggered-by event})

   (event/report :application/submit {:params params :triggered-by event})])
