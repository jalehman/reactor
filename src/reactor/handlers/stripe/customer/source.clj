(ns reactor.handlers.stripe.customer.source
  (:require [blueprints.models.account :as account]
            [blueprints.models.event :as event]
            [mailer.core :as mailer]
            [mailer.message :as mm]
            [reactor.dispatch :as dispatch]
            [reactor.handlers.common :refer :all]
            [reactor.handlers.stripe.common :as common]
            [reactor.services.slack :as slack]
            [reactor.services.slack.message :as sm]
            [reactor.utils.mail :as mail]
            [teller.customer :as tcustomer]
            [teller.event :as tevent]
            [teller.source :as tsource]
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


(defmulti notify
  "Notify the customer of a verification event."
  status-dispatch)


(defmethod notify :default [_ _ _] nil) ; nothing to do


(defmethod notify "verification_failed" [deps event {:keys [account-id]}]
  (let [account (td/entity account-id (->db deps))]
    (mailer/send
     (->mailer deps)
     (account/email account)
     (mail/subject "Bank Verification Failed")
     (mm/msg
      (mm/greet (account/first-name account))
      (mm/p "Unfortunately we were unable to make the two small deposits to the bank account you provided &mdash; it's likely that the information provided was incorrect.")
      (mm/p "Please log back in to Starcity by clicking "
            [:a {:href (->dashboard-hostname deps)} "this link"]
            " to re-enter your bank account information.")
      mail/accounting-sig)
     {:uuid (event/uuid event)
      :from mail/from-accounting})))


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
;; Delete Deleted
;; =============================================================================


(defmethod dispatch/stripe :stripe.event.customer.source/deleted
  [deps event params]
  (let [se (common/fetch-event (->teller deps) event)]
    (tevent/handle-stripe-event (->teller deps) se)))


;; =============================================================================
;; Source Updated
;; =============================================================================


(defmethod dispatch/stripe :stripe.event.customer.source/updated
  [deps event params]
  (let [se       (common/fetch-event (->teller deps) event)
        status   (:status se)
        source   (tsource/by-id (->teller deps) (:id (common/subject se)))
        _        (tevent/handle-stripe-event (->teller deps) se)
        customer (when-let [s source] (tsource/customer s))
        account  (when-let [c customer] (tcustomer/account c)) ]
    (when (tsource/bank-account? source)
      ((juxt event/report event/notify) (event/key event)
       {:params       {:account-id (td/id account)
                       :status     status}
        :triggered-by event}))))
