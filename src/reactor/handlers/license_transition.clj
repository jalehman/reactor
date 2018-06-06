(ns reactor.handlers.license-transition
  (:require [blueprints.models.account :as account]
            [blueprints.models.member-license :as member-license]
            [blueprints.models.license-transition :as license-transition]
            [blueprints.models.unit :as unit]
            [blueprints.models.event :as event]
            [datomic.api :as d]
            [mailer.core :as mailer]
            [mailer.message :as mm]
            [reactor.dispatch :as dispatch]
            [reactor.handlers.common :refer :all]
            [reactor.services.slack :as slack]
            [reactor.services.slack.message :as sm]
            [reactor.utils.mail :as mail]
            [toolbelt.datomic :as td]
            [toolbelt.date :as date]
            [taoensso.timbre :as timbre]
            [blueprints.models.property :as property]))


(defn- member-url [hostname account-id]
  (format "%s/accounts/%s" hostname account-id))


(defn- make-friendly-unit-name
  [unit]
  (let [code        (unit/code unit)
        property    (property/name (unit/property unit))
        unit-number (subs code (inc (clojure.string/last-index-of code "-")))]
    (str property " #" unit-number)))


(def ^:private property-channel
  {"52gilbert"   "#52-gilbert"
   "2072mission" "#2072-mission"
   "6nottingham" "#6-nottingham"
   "414bryant"   "#414-byant"})


(defn- notification-channel [property]
  (let [code (property/code property)]
    (get property-channel code slack/crm)))


;; slack notification -> staff
(defmethod dispatch/report :transition/move-out-created
  [deps event {:keys [transition-uuid] :as params}]
  (let [transition (license-transition/by-uuid (->db deps) transition-uuid)
        license    (license-transition/current-license transition)
        member     (member-license/account license)
        unit       (member-license/unit license)]

    (slack/send
     (->slack deps)
     {:uuid    (event/uuid event)
      :channel (notification-channel (unit/property unit))}
     (sm/msg
      (sm/info
       (sm/title (str (account/short-name member) " is moving out!")
                 (member-url (->dashboard-hostname deps) (td/id member)))
       (sm/text "Learn more about this member's move out in the Admin Dashboard.")
       (sm/fields
        (sm/field "Unit" (make-friendly-unit-name unit))
        (sm/field "Move-out date" (date/short (license-transition/date transition)))
        (when-let [a (:asana/task transition)]
          (sm/field "Asana Move-out Task" a))))))))


;; email notification -> member
(defmethod dispatch/notify :transition/move-out-created
  [deps event {:keys [transition-uuid] :as params}]
  (let [transition (license-transition/by-uuid (->db deps) transition-uuid)
        license    (license-transition/current-license transition)
        member     (member-license/account license)]

    (mailer/send
     (->mailer deps)
     (account/email member)
     (mail/subject (format "%s, we've begun processing your move-out." (account/first-name member)))
     (mm/msg
      (mm/greet (account/first-name member))
      (mm/p
       "We've received your notice of intent to move out. We're sad to see you go!")
      (mm/p
       (format "We have your move-out day set for %s. If this is incorrect, please reach out to your community representative so we can adjust it." (date/short (license-transition/date transition))))
      (mm/p "If you have any questions, please don't hesitate to ask your community representative.")
      (mm/sig))
     {:uuid (event/uuid event)})))


(defmethod dispatch/job :transition/move-out-created
  [deps event {:keys [transition-uuid] :as params}]
  (let [transition (license-transition/by-uuid (->db deps) transition-uuid)]
    [(event/report (event/key event) {:params       {:transition-uuid transition-uuid}
                                      :triggered-by event})
     (event/notify (event/key event) {:params       {:transition-uuid transition-uuid}
                                      :triggered-by event})]))


(defmethod dispatch/report :transition/move-out-updated
  [deps event {:keys [transition-id]  :as params}]
  (let [transition (d/entity (->db deps) transition-id)
        license    (license-transition/current-license transition)
        member     (member-license/account license)
        unit       (member-license/unit license)]

    (slack/send
     (->slack deps)
     {:uuid    (event/uuid event)
      :channel (notification-channel (unit/property unit))}
     (sm/msg
      (sm/info
       (sm/title (str "Updated Move-out Info for " (account/short-name member))
                 (member-url (->dashboard-hostname deps) (td/id member)))
       (sm/text "Learn more about this member's move out in the Admin Dashboard.")
       (sm/fields
        (sm/field "Unit" (make-friendly-unit-name unit))
        (sm/field "Move-out date" (date/short (license-transition/date transition)))
        (when-let [a (:asana/task transition)]
          (sm/field "Asana Move-out Task" a))))))))


(defmethod dispatch/job :transition/move-out-updated
  [deps event {:keys [transition-id] :as params}]
  [(event/report (event/key event) {:params {:transition-id transition-id}
                                    :triggered-by event})])


(defmethod dispatch/report :transition/renewal-created
  [deps event {:keys [transition-uuid] :as params}]
  (let [transition      (license-transition/by-uuid (->db deps) transition-uuid)
        current-license (license-transition/current-license transition)
        new-license     (license-transition/new-license transition)
        member          (member-license/account current-license)
        unit            (member-license/unit current-license)]

    (slack/send
     (->slack deps)
     {:uuid    (event/uuid event)
      :cahnnel (notification-channel (unit/property unit))}
     (sm/msg
      (sm/info
       (sm/title (str (account/short-name member) " has renewed their license!")
                 (member-url (->dashboard-hostname deps) (td/id member)))
       (sm/text "Learn more about this member's renewal in the Admin Dashboard.")
       (sm/fields
        (sm/field "Unit" (make-friendly-unit-name unit))
        (sm/field "New License Term" (member-license/term new-license))
        (sm/field "New License Rate" (member-license/rate new-license))
        (sm/field "Renewal date" (date/short (license-transition/date transition)))))))))


(defmethod dispatch/notify :transition/renewal-created
  [deps event {:keys [transition-uuid] :as params}]
  (let [transition      (license-transition/by-uuid (->db deps) transition-uuid)
        current-license (license-transition/current-license transition)
        new-license     (license-transition/new-license transition)
        member          (member-license/account current-license)
        unit            (member-license/unit current-license)]
    ;; TODO - send an email message
    (mailer/send
     (->mailer deps)
     (account/email member)
     (mail/subject (format "%s, your license has been renewed!" (account/first-name member)))
     (mm/msg
      (mm/greet (account/first-name member))
      (mm/p
       "Thank you for renewing your license with us!")
      (mm/p
       (format "Your new license will take effect on %s. You've committed to a %s month term at a rate of %s/month. If any of this information is incorrect, please reach out to your community representative so we can adjust it." (date/short (member-license/starts new-license)) (member-license/term new-license) (format/currency (member-license/rate new-license))))
      (mm/p "If you have any questions, please don't hesitate to ask your community representative.")
      (mm/sig))
     {:uuid (event/uuid event)})
    ))


(defmethod dispatch/job :transition/renewal-created
  [deps event {:keys [transition-uuid] :as params}]
  [(event/report (event/key event) {:params       {:transition-uuid transition-uuid}
                                    :triggered-by event})
   (event/notify (event/key event) {:params       {:transition-uuid transition-uuid}
                                    :triggered-by event})])
