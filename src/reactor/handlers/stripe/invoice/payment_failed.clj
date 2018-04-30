(ns reactor.handlers.stripe.invoice.payment-failed
  (:require [blueprints.models.account :as account]
            [blueprints.models.event :as event]
            [blueprints.models.order :as order]
            [blueprints.models.service :as service]
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


(defmethod dispatch/notify ::notify.rent [deps event {:keys [account-id]}]
  (let [account (d/entity (->db deps) account-id)]
    (mailer/send
     (->mailer deps)
     (account/email account)
     (mail/subject "Autopay Payment Failed")
     (mm/msg
      (mm/greet (account/first-name account))
      (mm/p "Unfortunately, your Autopay payment for this month's rent has failed.")
      (mm/p "We'll retry again <b>tomorrow</b>. In the meantime, please ensure that you have sufficient funds in the account that you have linked to Autopay.")
      mail/accounting-sig)
     {:uuid (event/uuid event)
      :from mail/from-accounting})))


(defmethod dispatch/notify ::notify.service [deps event {:keys [payment-id]}]
  (let [payment (tpayment/by-id (->teller deps) payment-id)
        order   (order/by-payment (->db deps) payment)
        service (order/service order)
        account (order/account order)]
    (mailer/send
     (->mailer deps)
     (account/email account)
     (mail/subject (format "Payment Failed for '%s'" (service/desc service)))
     (mm/msg
      (mm/greet (account/first-name account))
      (mm/p (format "Unfortunately, your recurring payment of $%.2f for <b>%s</b> has failed."
                    (tpayment/amount payment) (service/desc service)))
      (mm/p "We'll try the payment again within the next couple of days; in the meantime, please ensure that your payment source has sufficient funds.")
      mail/accounting-sig)
     {:uuid (event/uuid event)
      :from mail/from-accounting})))


(defmethod dispatch/notify ::notify.service.final [deps event {:keys [payment-id]}]
  (let [payment (tpayment/by-id (->teller deps) payment-id)
        order   (order/by-payment (->db deps) payment)
        service (order/service order)
        account (order/account order)]
    (mailer/send
     (->mailer deps)
     (account/email account)
     (mail/subject (format "Final Payment Failed for '%s'" (service/desc service)))
     (mm/msg
      (mm/greet (account/first-name account))
      (mm/p (format "Our final attempt to charge you for <b>%s</b> has failed."
                    (service/desc service)))
      (mm/p "Expect a member of our team to reach out shortly to coordinate next steps.")
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
     {:uuid    (event/uuid event)
      :channel slack/ops}
     (sm/msg
      (sm/failure
       (sm/title "Failed Rent Invoice")
       (sm/text (format "%s's autopay payment has failed" (account/full-name account))))))))


(defmethod dispatch/report ::notify.service [deps event {:keys [payment-id]}]
  (let [payment (tpayment/by-id (->teller deps) payment-id)
        order   (order/by-payment (->db deps) payment)
        service (order/service order)
        account (order/account order)]
    (slack/send
     (->slack deps)
     {:uuid    (event/uuid event)
      :channel slack/ops}
     (sm/msg
      (sm/failure
       (sm/title "Failed Service Invoice")
       (sm/text (format "%s's service payment has failed." (account/full-name account)))
       (sm/fields
        (sm/field "Account" (account/email account) true)
        (sm/field "Service" (service/desc service) true)
        (sm/field "Amount" (tpayment/amount payment) true)))))))


(defmethod dispatch/report ::notify.service.final [deps event {:keys [payment-id]}]
  (let [payment (tpayment/by-id (->teller deps) payment-id)
        order   (order/by-payment (->db deps) payment)
        service (order/service order)
        account (order/account order)]
    (slack/send
     (->slack deps)
     {:uuid    (event/uuid event)
      :channel slack/ops}
     (sm/msg
      (sm/failure
       (sm/title "Final Failed Service Invoice")
       (sm/text (format "%s's service payment has failed, and no subsequent attempts will be made. Please coordinate next steps with the customer."
                        (account/full-name account)))
       (sm/fields
        (sm/field "Account" (account/email account) true)
        (sm/field "Service" (service/desc service) true)
        (sm/field "Amount" (tpayment/amount payment) true)))))))


;; =============================================================================
;; Payment Failed
;; =============================================================================


(defmulti payment-failed ic/invoice-dispatch)


(defmethod payment-failed :default [_ event payment]
  (timbre/warn :stripe.event.invoice.payment-failed/unknown
               {:uuid         (event/uuid event)
                :payment      (tpayment/id payment)
                :subscription (-> payment tpayment/subscription tsubscription/id)}))


(defmethod payment-failed :payment.type/rent [deps event payment]
  (let [account (-> payment tpayment/customer tcustomer/account)]
    (mapv
     (fn [topic]
       (let [params {:account-id (td/id account)
                     :payment-id (tpayment/id payment)}]
         (event/create ::notify.rent {:params       params
                                      :triggered-by event
                                      :topic        topic})))
     [:notify :report])))


(defmethod payment-failed :payment.type/order [deps event payment]
  (let [account (-> payment tpayment/customer tcustomer/account)]
    (mapv
     (fn [topic]
       (let [params {:account-id (td/id account)
                     :payment-id (tpayment/id payment)}
             key    (if (tpayment/failed? payment)
                      ::notify.service.final ::notify.service)]
         (event/create key {:params       params
                            :triggered-by event
                            :topic        topic})))
     [:notify :report])))


(defmethod dispatch/stripe :stripe.event.invoice/payment-failed
  [deps event stripe-event]
  (let [se      (common/fetch-event (->teller deps) event)
        payment (tevent/handle-stripe-event (->teller deps) se)]
    (payment-failed deps event payment)))
