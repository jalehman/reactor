(ns reactor.handlers.stripe.customer.subscription.deleted
  (:require [blueprints.models.account :as account]
            [blueprints.models.event :as event]
            [datomic.api :as d]
            [mailer.core :as mailer]
            [mailer.message :as mm]
            [reactor.dispatch :as dispatch]
            [reactor.handlers.common :refer :all]
            [reactor.handlers.stripe.common :as common]
            [reactor.services.slack :as slack]
            [reactor.services.slack.message :as sm]
            [reactor.utils.mail :as mail]
            [taoensso.timbre :as timbre]
            [teller.customer :as tcustomer]
            [teller.event :as tevent]
            [teller.subscription :as tsubscription]
            [toolbelt.datomic :as td]))

;; =============================================================================
;; Notify
;; =============================================================================


(defmethod dispatch/notify :stripe.event.customer.subscription.deleted/rent
  [deps event {:keys [account-id]}]
  (let [account (d/entity (->db deps) account-id)]
    (mailer/send
     (->mailer deps)
     (account/email account)
     (mail/subject "Autopay Deactivated")
     (mm/msg
      (mm/greet (account/first-name account))
      (mm/p "Autopay has been deactivated for your account.")
      (mm/p "If this comes as a surprise, please log in to your member dashboard or reach out to your community manager for help.")
      mail/accounting-sig)
     {:uuid (event/uuid event)
      :from mail/from-accounting})))


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
        (sm/field "Account" (account-link (->dashboard-hostname deps) account))))))))


;; =============================================================================
;; Subscription Deleted
;; =============================================================================


(defmulti subscription-deleted
  (fn [deps event sub]
    (tsubscription/payment-type sub)))


(defmethod subscription-deleted :default [_ event sub]
  (timbre/warn :stripe.event.customer.subscription/deleted
               {:uuid         (event/uuid event)
                :subscription (tsubscription/id sub)}))


(defmethod subscription-deleted :payment.type/rent [deps event sub]
  (let [account (-> sub tsubscription/customer tcustomer/account)
        eparams {:params       {:account-id (td/id account)}
                 :triggered-by event}]
    [(event/notify :stripe.event.customer.subscription.deleted/rent eparams)
     (event/report :stripe.event.customer.subscription.deleted/rent eparams)]))


(defmethod dispatch/stripe :stripe.event.customer.subscription/deleted
  [deps event params]
  (let [se  (common/fetch-event (->teller deps) event)
        sub (tevent/handle-stripe-event (->teller deps) se)]
    (subscription-deleted deps event sub)))
