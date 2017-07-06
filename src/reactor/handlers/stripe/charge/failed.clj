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
            [toolbelt.datomic :as td]
            [ribbon.event :as re]
            [datomic.api :as d]))

;; =============================================================================
;; Helpers
;; =============================================================================


(defn charge-link [id]
  (format "https://dashboard.stripe.com/payments/%s" id))


;; =============================================================================
;; Reports
;; =============================================================================


(defmethod dispatch/report ::notify.deposit [deps event {:keys [account-id charge-id]}]
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


(defmethod dispatch/report ::notify.rent [deps event {:keys [account-id charge-id]}]
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


(defmethod dispatch/report ::notify.service [deps event {:keys [account-id charge-id]}]
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


;; =============================================================================
;; Notify Events
;; =============================================================================


(defn- retry-link [deps charge]
  (let [deposit (deposit/by-charge charge)]
    (if (deposit/partially-paid? deposit)
      (format "%s/me/account/rent" (->public-hostname deps))
      (format "%s/onboarding" (->public-hostname deps)))))


(defmethod dispatch/notify ::notify.deposit [deps event {:keys [account-id charge-id]}]
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


(defmethod dispatch/notify ::notify.rent [deps event {:keys [account-id charge-id]}]
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


(defmethod dispatch/notify ::notify.service [deps event {:keys [account-id charge-id]}]
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


;; =============================================================================
;; Process


(defn- notify-params [charge]
  {:account-id (td/id (charge/account charge))
   :charge-id  (td/id charge)})


(defn notify-events [key charge event]
  (mapv
   (fn [topic]
     (event/create key
                   {:params       (notify-params charge)
                    :triggered-by event
                    :topic        topic}))
   [:report :notify]))


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
    (concat
     (notify-events ::notify.rent charge event)
     [(rent-payment/set-due payment)
      [:db/retract (td/id payment) :rent-payment/paid-on (rent-payment/paid-on payment)]])))


(defmethod process-failed-charge :security-deposit [deps charge event]
  (notify-events ::notify.deposit charge event))


(defmethod process-failed-charge :service [deps charge event]
  (let [payment (payment/by-charge-id (->db deps) (charge/id charge))]
    (-> (notify-events ::notify.service charge event)
        (conj (payment/is-failed payment)))))


(defn- invoice-charge? [stripe-event]
  (-> stripe-event re/subject :invoice some?))


(defmethod dispatch/stripe :stripe.event.charge/failed [deps event _]
  (let [se (common/fetch-event (->stripe deps) event)
        ch (charge/by-id (->db deps) (re/subject-id se))]
    (assert (not (charge/failed? ch))
            "Charge has already failed; not processing.")
    ;; don't bother processing charges that belong to invoices
    (when-not (invoice-charge? se)
      (concat
       (process-failed-charge deps ch event)
       [(charge/failed ch)]))))
