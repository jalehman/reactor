(ns reactor.handlers.stripe.charge.failed
  (:require [blueprints.models
             [account :as account]
             [charge :as charge]
             [order :as order]
             [payment :as payment]
             [rent-payment :as rent-payment]
             [security-deposit :as deposit]
             [service :as service]]
            [mailer
             [core :as mailer]
             [message :as mm]]
            [reactor.dispatch :as dispatch]
            [reactor.handlers.common :refer :all]
            [reactor.handlers.stripe.common :as common]
            [reactor.models.event :as event]
            [reactor.services.slack :as slack]
            [reactor.services.slack.message :as sm]
            [taoensso.timbre :as timbre]
            [toolbelt.datomic :as td]))

;; =============================================================================
;; Helpers
;; =============================================================================


(defn charge-link [id]
  (format "https://dashboard.stripe.com/payments/%s" id))


;; =============================================================================
;; Internal Notification (slack)
;; =============================================================================


(defmulti notify-internal
  "Notify us that a customer's charge has failed."
  (fn [deps event params]
    (:type params)))


(defmethod notify-internal :default [_ _ _] nil) ; nothing to do


(defmethod notify-internal :security-deposit
  [deps event {:keys [account-id charge-id]}]
  (let [[account charge] (td/entities (->db deps) account-id charge-id)
        deposit          (deposit/by-account account)]
    (slack/send
     (->slack deps)
     {:uuid    (event/uuid event)
      :channel slack/ops}
     (sm/msg
      (sm/failure
       (sm/title "Security Deposit ACH Failure" (charge-link (charge/id charge)))
       (sm/text (format "%s's ACH payment has failed." (account/full-name account)))
       (sm/fields
        (sm/field "Email" (account/email account) true)
        (sm/field "Payment" (if (deposit/partially-paid? deposit) "remainder" "initial") true)
        (sm/field "Amount" (format "$%.2f" (charge/amount charge)) true)))))))


(defmethod notify-internal :rent
  [deps event {:keys [account-id charge-id]}]
  (let [[account charge] (td/entities (->db deps) account-id charge-id)]
    (slack/send
     (->slack deps)
     {:uuid    (event/uuid event)
      :channel slack/ops}
     (sm/msg
      (sm/failure
       (sm/title "ACH Rent Payment Failed" (charge-link (charge/id charge)))
       (sm/text (format "%s's rent payment has failed to go through."
                        (account/full-name account)))
       (sm/fields
        (sm/field "Amount" (format "$%.2f" (charge/amount charge)) true)
        (sm/field "Email" (account/email account) true)))))))


(defmethod notify-internal :service
  [deps event {:keys [account-id charge-id]}]
  (let [[account charge] (td/entities (->db deps) account-id charge-id)
        payment          (payment/by-charge-id (->db deps) (charge/id charge))
        order            (order/by-payment (->db deps) payment)]
    (slack/send
     (->slack deps)
     {:uuid    (event/uuid event)
      :channel slack/ops}
     (sm/msg
      (sm/failure
       (sm/title "Service Charge Failed" (charge-link (payment/charge-id payment)))
       (sm/text (format "%s's charge for *%s* failed."
                        (-> order order/service service/name)
                        (account/full-name account)))
       (sm/fields
        (sm/field "Amount" (format "$%.2f" (payment/amount payment)) true)
        (sm/field "Email" (account/email account) true)))))))


(defmethod dispatch/slack :stripe.event.charge.failed/notify-internal
  [deps event params]
  (notify-internal deps event params))


;; =============================================================================
;; Email Notification


;; TODO: Test
(defmulti notify-customer
  "Notify the customer that his/her charge has failed."
  (fn [deps event params]
    (:type params)))


(defmethod notify-customer :default [_ _ _] nil) ; nothing to do


(defn- retry-link [deps charge]
  (let [deposit (deposit/by-charge charge)]
    (if (deposit/partially-paid? deposit)
      (format "%s/me/account/rent" (->public-hostname deps))
      (format "%s/onboarding" (->public-hostname deps)))))


(defmethod notify-customer :security-deposit
  [deps event {:keys [account-id charge-id]}]
  (let [[account charge] (td/entities (->db deps) account-id charge-id)]
    (mailer/send
     (->mailer deps)
     (account/email account)
     "Starcity: Security Deposit Payment Failure"
     (mm/msg
      (mm/greet (account/first-name account))
      (mm/p "Unfortunately your security deposit payment failed to go through.")
      (mm/p "The most common reason for this are insufficient funds.")
      ;; If it's partially paid, that means that the user is no longer
      ;; in onboarding.
      (mm/p "Please log back in to Starcity by clicking "
            [:a {:href (retry-link deps charge)} "this link"]
            " to retry your payment.")
      (mm/sig))
     {:uuid (event/uuid event)})))


(defmethod notify-customer :rent
  [deps event {:keys [account-id charge-id]}]
  (let [[account charge] (td/entities (->db deps) account-id charge-id)
        payment          (rent-payment/by-charge (->db deps) charge)]
    (mailer/send
     (->mailer deps)
     (account/email account)
     "Starcity: Rent Payment Failed"
     (mm/msg
      (mm/greet (account/first-name account))
      (mm/p "Unfortunately your rent payment has failed to go through.")
      (mm/p (format "Please log back into your <a href='%s/me/account/rent'>member dashboard</a> and try your payment again."
                    (->public-hostname deps)))
      (mm/sig))
     {:uuid (event/uuid event)})))


(defmethod notify-customer :service
  [deps event {:keys [account-id charge-id]}]
  (let [[account charge] (td/entities (->db deps) account-id charge-id)
        payment          (payment/by-charge-id (->db deps) (charge/id charge))
        order            (order/by-payment (->db deps) payment)]
    (mailer/send
     (->mailer deps)
     (account/email account)
     "Starcity: Service Charge Failed"
     (mm/msg
      (mm/greet (account/first-name account))
      (mm/p (format "Unfortunately your payment for '%s' has failed to go through."
                    (-> order order/service service/name)))
      (mm/p "Your payment will be retried soon, so please ensure that the credit/debit card you provided is the one that we should charge.")
      (mm/p (format "You can log back into your member dashboard <a href='%s/me'>here</a> to update your payment information."
                    (->public-hostname deps)))
      (mm/sig))
     {:uuid (event/uuid event)})))


(defmethod dispatch/mail :stripe.event.charge.failed/notify-customer
  [deps event params]
  (notify-customer deps event params))


;; =============================================================================
;; Process


(defn- notify-event
  [key topic]
  (fn [triggered-by charge type]
    (event/create key
                  {:params       {:account-id (td/id (charge/account charge))
                                  :charge-id  (td/id charge)
                                  :type       type}
                   :topic        topic
                   :triggered-by triggered-by})))


(def notify-internal-event
  (notify-event :stripe.event.charge.failed/notify-internal :slack))


(def notify-customer-event
  (notify-event :stripe.event.charge.failed/notify-customer :mail))


(defmulti process-failed-charge
  "Using the charge's type, produce a transaction to update any entities (if
  any) that need to be updated in the db."
  (fn [deps charge event]
    (charge/type (->db deps) charge)))


(defmethod process-failed-charge :unknown [deps charge event]
  (timbre/warn :stripe.event.charge.failed/unknown
               {:uuid   (event/uuid event)
                :charge (charge/id charge)}))


(defmethod process-failed-charge :rent [deps charge event]
  (let [payment (rent-payment/by-charge (->db deps) charge)]
    [(rent-payment/set-due payment)
     [:db/retract (td/id payment) :rent-payment/paid-on (rent-payment/paid-on payment)]
     (notify-internal-event event charge :rent)
     (notify-customer-event event charge :rent)]))


(defmethod process-failed-charge :security-deposit [deps charge event]
  [(notify-internal-event event charge :security-deposit)
   (notify-customer-event event charge :security-deposit)])


(defmethod process-failed-charge :service [deps charge event]
  (let [payment (payment/by-charge-id (->db deps) (charge/id charge))]
    [(payment/is-failed payment)
     (notify-internal-event event charge :service)
     (notify-customer-event event charge :service)]))


(defmethod dispatch/stripe :stripe.event.charge/failed [deps event _]
  (let [se (common/fetch-event (->stripe deps) event)
        ch (charge/by-id (->db deps) (common/event-subject-id se))]
    (assert (not (charge/failed? ch)) "Charge has already failed; not processing.")
    (concat
     (process-failed-charge deps ch event)
     [(charge/failed ch)])))
