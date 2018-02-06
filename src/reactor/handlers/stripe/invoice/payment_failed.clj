(ns reactor.handlers.stripe.invoice.payment-failed
  (:require [blueprints.models.account :as account]
            [blueprints.models.event :as event]
            [blueprints.models.member-license :as member-license]
            [blueprints.models.order :as order]
            [blueprints.models.payment :as payment]
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
            [ribbon.event :as re]
            [taoensso.timbre :as timbre]))

;; =============================================================================
;; Notify
;; =============================================================================


(defmethod dispatch/notify ::notify.rent [deps event {:keys [member-license-id]}]
  (let [license (d/entity (->db deps) member-license-id)
        account (member-license/account license)]
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


(defmethod dispatch/notify ::notify.service [deps event {:keys [invoice]}]
  (let [payment (payment/by-invoice-id (->db deps) invoice)
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
                    (order/computed-price order) (service/desc service)))
      (mm/p "We'll try the payment again within the next couple of days; in the meantime, please ensure that your payment source has sufficient funds.")
      mail/accounting-sig)
     {:uuid (event/uuid event)
      :from mail/from-accounting})))


(defmethod dispatch/notify ::notify.service.final [deps event {:keys [invoice]}]
  (let [payment (payment/by-invoice-id (->db deps) invoice)
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


(defmethod dispatch/report ::notify.rent [deps event {:keys [invoice]}]
  (let [payment (payment/by-invoice-id (->db deps) invoice)
        license (member-license/by-invoice-id (->db deps) invoice)
        account (member-license/account license)
        managed (member-license/rent-connect-id license)]
    (slack/send
     (->slack deps)
     {:uuid    (event/uuid event)
      :channel slack/ops}
     (sm/msg
      (sm/failure
       (sm/title "Failed Rent Invoice" (ic/invoice-dashboard-url managed invoice))
       (sm/text (format "%s's autopay payment has failed" (account/full-name account))))))))


(defmethod dispatch/report ::notify.service [deps event {:keys [invoice]}]
  (let [payment (payment/by-invoice-id (->db deps) invoice)
        order   (order/by-payment (->db deps) payment)
        service (order/service order)
        account (order/account order)]
    (slack/send
     (->slack deps)
     {:uuid    (event/uuid event)
      :channel slack/ops}
     (sm/msg
      (sm/failure
       (sm/title "Failed Service Invoice" (ic/invoice-dashboard-url invoice))
       (sm/text (format "%s's service payment has failed." (account/full-name account)))
       (sm/fields
        (sm/field "Account" (account/email account) true)
        (sm/field "Service" (service/desc service) true)
        (sm/field "Amount" (order/computed-price order) true)))))))


(defmethod dispatch/report ::notify.service.final [deps event {:keys [invoice]}]
  (let [payment (payment/by-invoice-id (->db deps) invoice)
        order   (order/by-payment (->db deps) payment)
        service (order/service order)
        account (order/account order)]
    (slack/send
     (->slack deps)
     {:uuid    (event/uuid event)
      :channel slack/ops}
     (sm/msg
      (sm/failure
       (sm/title "Final Failed Service Invoice" (ic/invoice-dashboard-url invoice))
       (sm/text (format "%s's service payment has failed, and no subsequent attempts will be made. Please coordinate next steps with the customer."
                        (account/full-name account)))
       (sm/fields
        (sm/field "Account" (account/email account) true)
        (sm/field "Service" (service/desc service) true)
        (sm/field "Amount" (order/computed-price order) true)))))))


;; =============================================================================
;; Payment Failed
;; =============================================================================


(def ^:private max-payment-attempts 3)


(defmulti payment-failed ic/invoice-dispatch)


(defmethod payment-failed :default [_ event stripe-event]
  (timbre/warn :stripe.event.invoice.payment-failed/unknown
               {:uuid         (event/uuid event)
                :invoice      (re/subject-id stripe-event)
                :subscription (ic/subs-id stripe-event)}))


(defn- max-attempts-exceeded? [stripe-event]
  (let [attempt-count (:attempt_count (re/subject stripe-event))]
    (>= attempt-count max-payment-attempts)))


(defn- autopay->normal-payment
  "Remove autopay-specific attributes of the `payment`"
  [payment]
  [[:db/retract payment :payment/paid-on (payment/paid-on payment)]
   [:db/retract payment :stripe/invoice-id (payment/invoice-id payment)]
   [:db/add payment :payment/status :payment.status/failed]])


(defmethod payment-failed :rent [deps event stripe-event]
  (let [invoice-id (re/subject-id stripe-event)
        payment    (payment/by-invoice-id (->db deps) invoice-id)
        license    (member-license/by-invoice-id (->db deps) invoice-id)]
    (if (max-attempts-exceeded? stripe-event)
      (autopay->normal-payment payment)
      (mapv
       (fn [topic]
         (let [params {:member-license-id (:db/id license)
                       :invoice           (re/subject-id stripe-event)}]
           (event/create ::notify.rent {:params       params
                                        :triggered-by event
                                        :topic        topic})))
       [:notify :report]))))


(defmethod payment-failed :service [deps event stripe-event]
  (let [payment (payment/by-invoice-id (->db deps) (re/subject-id stripe-event))]
    (-> (mapv
         (fn [topic]
           (let [params {:invoice (re/subject-id stripe-event)}
                 key    (if (max-attempts-exceeded? stripe-event)
                          ::notify.service.final ::notify.service)]
             (event/create key {:params       params
                                :triggered-by event
                                :topic        topic})))
         [:notify :report])
        (conj (payment/is-failed payment)))))


(defmethod dispatch/stripe :stripe.event.invoice/payment-failed
  [deps event stripe-event]
  (let [stripe-event (common/fetch-event (->stripe deps) event)]
    (payment-failed deps event stripe-event)))
