(ns reactor.handlers.stripe.invoice.payment-succeeded
  (:require [blueprints.models.account :as account]
            [blueprints.models.event :as event]
            [datomic.api :as d]
            [mailer.core :as mailer]
            [mailer.message :as mm]
            [reactor.dispatch :as dispatch]
            [reactor.handlers.common :refer :all]
            [reactor.handlers.stripe.common :as common]
            [reactor.handlers.stripe.invoice.common :as ic]
            [reactor.services.slack :as slack]
            [reactor.services.slack.message :as sm]
            [reactor.utils.mail :as mail]
            [taoensso.timbre :as timbre]
            [teller.customer :as tcustomer]
            [teller.event :as tevent]
            [teller.payment :as tpayment]
            [teller.subscription :as tsubscription]
            [toolbelt.datomic :as td]))

;; =============================================================================
;; Notify
;; =============================================================================


(defmethod dispatch/notify ::notify.rent [deps event {:keys [payment-id account-id]}]
  (let [account (d/entity (->db deps) account-id)
        payment (tpayment/by-id (->teller deps) payment-id)]
    (mailer/send
     (->mailer deps)
     (account/email account)
     (mail/subject "Autopay Payment Successful")
     (mm/msg
      (mm/greet (account/first-name account))
      (mm/p
       (format "This is a friendly reminder to let you know that your rent payment of $%.2f has been successfully paid."
               (tpayment/amount payment)))
      (mm/p "Thanks for using Autopay!")
      mail/accounting-sig)
     {:uuid (event/uuid event)
      :from mail/from-accounting})))


;; =============================================================================
;; Report
;; =============================================================================


(defmethod dispatch/report ::notify.rent [deps event {:keys [account-id]}]
  (let [account (d/entity (->db deps) account-id)]
    (slack/send
     (->slack deps)
     {:channel slack/ops
      :uuid    (event/uuid event)}
     (sm/msg
      (sm/success
       (sm/title "Autopay Success")
       (sm/text (format "%s's autopay payment has succeeded!"
                        (account/full-name account))))))))


;; =============================================================================
;; Payment Succeeded
;; =============================================================================


(defmulti payment-succeeded ic/invoice-dispatch)


(defmethod payment-succeeded :default [_ event payment]
  (timbre/warn :stripe.event.invoice.payment-succeeded/unknown
               {:uuid         (event/uuid event)
                :payment      (tpayment/id payment)
                :subscription (-> payment tpayment/subscription tsubscription/id)}))


(defmethod payment-succeeded :payment.type/rent [deps event payment]
  (let [account (-> payment tpayment/customer tcustomer/account)]
    (mapv
     (fn [topic]
       (let [params {:account-id (td/id account)
                     :payment-id (tpayment/id payment)}]
         (event/create ::notify.rent {:params       params
                                      :triggered-by event
                                      :topic        topic})))
     [:report :notify])))


(defmethod dispatch/stripe :stripe.event.invoice/payment-succeeded
  [deps event stripe-event]
  (let [se      (common/fetch-event (->teller deps) event)
        payment (tevent/handle-stripe-event (->teller deps) se)]
    (payment-succeeded deps event payment)))
