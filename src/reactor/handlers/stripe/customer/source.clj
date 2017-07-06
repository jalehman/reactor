(ns reactor.handlers.stripe.customer.source
  (:require [blueprints.models.account :as account]
            [mailer.core :as mailer]
            [mailer.message :as mm]
            [reactor.dispatch :as dispatch]
            [reactor.handlers.common :refer :all]
            [reactor.handlers.stripe.common :as common]
            [reactor.models.event :as event]
            [reactor.services.slack :as slack]
            [reactor.services.slack.message :as sm]
            [ribbon.event :as re]
            [toolbelt.datomic :as td]))


;; =============================================================================
;; Helpers
;; =============================================================================


(defn- status-dispatch
  [deps event params]
  (:status params))


;; =============================================================================
;; Notify
;; =============================================================================


(defn- link [account hostname]
  (cond
    (account/onboarding? account) (format "%s/onboarding" hostname)
    (account/member? account)     (format "%s/me/account/rent" hostname)
    :otherwise                    (throw (ex-info "Invalid role." {:role (account/role account)}))))


(defmulti notify
  "Notify the customer of a verification event."
  status-dispatch)


(defmethod notify :default [_ _ _] nil) ; nothing to do


(defmethod notify "verification_failed" [deps event {:keys [account-id]}]
  (let [account (td/entity account-id (->db deps))]
    (mailer/send
     (->mailer deps)
     (account/email account)
     "Starcity: Bank Verification Failed"
     (mm/msg
      (mm/greet (account/first-name account))
      (mm/p "Unfortunately we were unable to make the two small deposits to the bank account you provided &mdash; it's likely that the information provided was incorrect.")
      (mm/p "Please log back in to Starcity by clicking "
            [:a {:href (link account (->public-hostname deps))} "this link"]
            " to re-enter your bank account information.")
      (mm/sig))
     {:uuid (event/uuid event)})))


(defmethod dispatch/notify :stripe.event.customer.source/updated
  [deps event params]
  (notify deps event params))


;; =============================================================================
;; Report
;; =============================================================================


(defmulti report
  "Notify our team of a verification event."
  status-dispatch)


(defmethod report :default [_ _ _] nil)


(defmethod report "verified" [deps event {:keys [account-id]}]
  (let [account (td/entity account-id (->db deps))]
    (slack/send
     (->slack deps)
     {:uuid    (event/uuid event)
      :channel slack/ops}
     (sm/msg
      (sm/success
       (sm/title "Bank Verification Succeeded")
       (sm/text (format "%s has verified their bank account."
                        (account/full-name account))))))))


(defmethod report "verification_failed" [deps event {:keys [account-id]}]
  (let [account (td/entity account-id (->db deps))]
    (slack/send
     (->slack deps)
     {:uuid    (event/uuid event)
      :channel slack/ops}
     (sm/msg
      (sm/failure
       (sm/title "Bank Verification Failed")
       (sm/text (format "%s's bank verification has failed."
                        (account/full-name account))))))))


(defmethod dispatch/report :stripe.event.customer.source/updated
  [deps event params]
  (report deps event params))


;; =============================================================================
;; Transactions
;; =============================================================================


(defmethod dispatch/stripe :stripe.event.customer.source/updated
  [deps event params]
  (let [stripe-event                     (common/fetch-event (->stripe deps) event)
        {:keys [object status customer]} (re/subject stripe-event)
        account                          (account/by-customer-id (->db deps) customer)]
    ;; NOTE: Is deleting the customer even necessary? Perform some
    ;; experimentation with this flow later.
    (when (= object "bank_account")
      (cond-> ((juxt event/report event/notify) (event/key event)
               {:params       {:account-id (td/id account)
                               :status     status}
                :triggered-by event})
        (= status "verification_failed")
        (conj [:db.fn/retractEntity [:stripe-customer/customer-id customer]])))))
