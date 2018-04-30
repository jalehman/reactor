(ns reactor.handlers.stripe.charge.failed
  (:require [blueprints.models.account :as account]
            [blueprints.models.event :as event]
            [blueprints.models.order :as order]
            [blueprints.models.security-deposit :as deposit]
            [blueprints.models.service :as service]
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
            [teller.payment :as tpayment]
            [toolbelt.datomic :as td]))

;; =============================================================================
;; Reports
;; =============================================================================


(defmethod dispatch/report ::notify.deposit [deps event {:keys [account-id payment-id]}]
  (let [account (d/entity (->db deps) account-id)
        payment (tpayment/by-id (->teller deps) payment-id)
        deposit (deposit/by-account account)]
    (slack/send
     (->slack deps)
     {:uuid    (event/uuid event)
      :channel slack/ops}
     (sm/msg
      (sm/failure
       (sm/title "Security Deposit ACH Failure")
       (sm/text (format "%s's ACH payment has failed." (account/full-name account)))
       (sm/fields
        (sm/field "Email" (account/email account) true)
        (sm/field "Payment" (if (deposit/partially-paid? deposit) "remainder" "initial") true)
        (sm/field "Amount" (format "$%.2f" (tpayment/amount payment)) true)))))))


(defmethod dispatch/report ::notify.rent [deps event {:keys [account-id payment-id]}]
  (let [account (d/entity (->db deps) account-id)
        payment (tpayment/by-id (->teller deps) payment-id)]
    (slack/send
     (->slack deps)
     {:uuid    (event/uuid event)
      :channel slack/ops}
     (sm/msg
      (sm/failure
       (sm/title "ACH Rent Payment Failed")
       (sm/text (format "%s's rent payment has failed to go through."
                        (account/full-name account)))
       (sm/fields
        (sm/field "Amount" (format "$%.2f" (tpayment/amount payment)) true)
        (sm/field "Email" (account/email account) true)))))))


(defmethod dispatch/report ::notify.service [deps event {:keys [account-id payment-id]}]
  (let [account (d/entity (->db deps) account-id)
        payment (tpayment/by-id (->teller deps) payment-id)
        order   (order/by-payment (->db deps) payment)]
    (slack/send
     (->slack deps)
     {:uuid    (event/uuid event)
      :channel slack/ops}
     (sm/msg
      (sm/failure
       (sm/title "Service Charge Failed")
       (sm/text (format "%s's charge for *%s* failed."
                        (-> order order/service service/name)
                        (account/full-name account)))
       (sm/fields
        (sm/field "Amount" (format "$%.2f" (tpayment/amount payment)) true)
        (sm/field "Email" (account/email account) true)))))))


;; =============================================================================
;; Notify Events
;; =============================================================================


(defmethod dispatch/notify ::notify.deposit [deps event {:keys [account-id payment-id]}]
  (let [account (d/entity (->db deps) account-id)
        payment (tpayment/by-id (->teller deps) payment-id)]
    (mailer/send
     (->mailer deps)
     (account/email account)
     (mail/subject "Security Deposit Payment Failure")
     (mm/msg
      (mm/greet (account/first-name account))
      (mm/p "Unfortunately your security deposit payment failed to go through.")
      (mm/p "The most common reason for this are insufficient funds.")
      (mm/p "Please log back in to Starcity by clicking "
            [:a {:href (->dashboard-hostname deps)} "this link"]
            " to retry your payment.")
      mail/accounting-sig)
     {:uuid (event/uuid event)
      :from mail/from-accounting})))


(defmethod dispatch/notify ::notify.rent [deps event {:keys [account-id payment-id]}]
  (let [account (d/entity (->db deps) account-id)
        payment (tpayment/by-id (->teller deps) payment-id)]
    (mailer/send
     (->mailer deps)
     (account/email account)
     (mail/subject "Rent Payment Failed")
     (mm/msg
      (mm/greet (account/first-name account))
      (mm/p "Unfortunately your rent payment has failed to go through.")
      (mm/p (format "Please log back into your <a href='%s/profile'>member dashboard</a> and try your payment again."
                    (->dashboard-hostname deps)))
      mail/accounting-sig)
     {:uuid (event/uuid event)
      :from mail/from-accounting})))


(defmethod dispatch/notify ::notify.service [deps event {:keys [account-id payment-id]}]
  (let [account (d/entity (->db deps) account-id)
        payment (tpayment/by-id (->teller deps) payment-id)
        order   (order/by-payment (->db deps) payment)]
    (mailer/send
     (->mailer deps)
     (account/email account)
     (mail/subject "Premium Service Charge Failed")
     (mm/msg
      (mm/greet (account/first-name account))
      (mm/p (format "Unfortunately your payment for '%s' has failed to go through."
                    (-> order order/service service/name)))
      (mm/p "Your payment will be retried soon, so please ensure that the credit/debit card you provided is the one that we should charge.")
      (mm/p (format "You can log back into your member dashboard <a href='%s/profile'>here</a> to update your payment information."
                    (->dashboard-hostname deps)))
      mail/accounting-sig)
     {:uuid (event/uuid event)
      :from mail/from-accounting})))


;; =============================================================================
;; Process


(defn notify-events [key payment event]
  (mapv
   (fn [topic]
     (let [account (tcustomer/account (tpayment/customer payment))]
       (event/create key
                     {:params       {:account-id (td/id account)
                                     :payment-id (tpayment/id payment)}
                      :triggered-by event
                      :topic        topic})))
   [:report :notify]))


(defmulti process-failed-charge
  (fn [deps payment event]
    (tpayment/type payment)))


(defmethod process-failed-charge :default [deps _ event]
  (timbre/warn :stripe.event.charge.failed/unknown
               {:uuid (event/uuid event)}))


(defmethod process-failed-charge :payment.type/rent [deps payment event]
  (notify-events ::notify.rent payment event))


(defmethod process-failed-charge :payment.type/deposit [deps payment event]
  (notify-events ::notify.deposit payment event))


(defmethod process-failed-charge :payment.type/order [deps payment event]
  (notify-events ::notify.service payment event))


(defmethod dispatch/stripe :stripe.event.charge/failed [deps event _]
  (let [se      (common/fetch-event (->teller deps) event)
        payment (tevent/handle-stripe-event (->teller deps) se)]
    (when-not (some? (tpayment/subscription payment))
      (process-failed-charge deps payment event))))
