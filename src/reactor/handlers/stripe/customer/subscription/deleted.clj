(ns reactor.handlers.stripe.customer.subscription.deleted
  (:require [blueprints.models.event :as event]
            [reactor.dispatch :as dispatch]
            [reactor.handlers.common :refer :all]
            [reactor.handlers.stripe.common :as common]
            [ribbon.event :as re]
            [taoensso.timbre :as timbre]
            [blueprints.models.member-license :as member-license]
            [toolbelt.datomic :as td]
            [datomic.api :as d]
            [mailer.core :as mailer]
            [blueprints.models.account :as account]
            [mailer.message :as mm]
            [reactor.services.slack :as slack]
            [reactor.services.slack.message :as sm]))


;; =============================================================================
;; Notify
;; =============================================================================


(defmethod dispatch/notify :stripe.event.customer.subscription.deleted/rent
  [deps event {:keys [account-id]}]
  (let [account (d/entity (->db deps) account-id)]
    (mailer/send
     (->mailer deps)
     (account/email account)
     "Starcity: Autopay Deactivated"
     (mm/msg
      (mm/greet (account/first-name account))
      (mm/p "Autopay has been deactivated for your account.")
      (mm/p "If this comes as a surprise, please log in to your member dashboard or reach out to your community manager for help.")
      (mm/sig))
     {:uuid (event/uuid event)})))


;; =============================================================================
;; Report
;; =============================================================================


(defmethod dispatch/report :stripe.event.customer.subscription.deleted/rent
  [deps event {:keys [account-id]}]
  (let [account (d/entity (->db deps) account-id)]
    (slack/send
     (->slack deps)
     {:uuid    (event/uuid event)
      :channel slack/ops}
     (sm/msg
      (sm/info
       (sm/title "Autopay Deactivated")
       (sm/text (format "%s's Autopay has been deactivated." (account/full-name account)))
       (sm/fields
        (sm/field "Account" (account-link (->public-hostname deps) account))))))))


;; =============================================================================
;; Subscription Deleted
;; =============================================================================


(defmulti subscription-deleted
  (fn [deps event stripe-event]
    (common/subscription-type (->db deps) (re/subject-id stripe-event))))


(defmethod subscription-deleted :default [_ event stripe-event]
  (timbre/warn :stripe.event.customer.subscription/deleted
               {:uuid         (event/uuid event)
                :subscription (re/subject-id stripe-event)}))


(defmethod subscription-deleted :rent [deps event stripe-event]
  (let [subs-id (re/subject-id stripe-event)
        license (member-license/by-subscription-id (->db deps) subs-id)
        eparams {:params       {:account-id (-> license member-license/account td/id)}
                 :triggered-by event}]
    [[:db/retract (td/id license) :member-license/subscription-id subs-id]
     (event/notify :stripe.event.customer.subscription.deleted/rent eparams)
     (event/report :stripe.event.customer.subscription.deleted/rent eparams)]))


(defmethod dispatch/stripe :stripe.event.customer.subscription/deleted
  [deps event params]
  (let [stripe-event (common/fetch-event (->stripe deps) event)]
    (subscription-deleted deps event stripe-event)))
