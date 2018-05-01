(ns reactor.handlers.stripe.invoice.created
  (:require [blueprints.models.account :as account]
            [blueprints.models.event :as event]
            [blueprints.models.order :as order]
            [blueprints.models.service :as service]
            [clojure.spec.alpha :as s]
            [datomic.api :as d]
            [mailer.core :as mailer]
            [mailer.message :as mm]
            [reactor.dispatch :as dispatch]
            [reactor.handlers.common :refer :all]
            [reactor.handlers.stripe.common :as common]
            [reactor.handlers.stripe.invoice.common :as ic]
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
     (mail/subject "Autopay Payment Pending")
     (mm/msg
      (mm/greet (account/first-name account))
      (mm/p "This is a friendly reminder that your autopay payment is being processed.")
      (mm/p "Please note that it may take up to <b>7 business days</b> for the funds to be withdrawn from your account.")
      mail/accounting-sig)
     {:uuid (event/uuid event)
      :from mail/from-accounting})))


(defmethod dispatch/notify ::notify.service [deps event {:keys [order-id]}]
  (let [order   (d/entity (->db deps) order-id)
        service (order/service order)
        account (order/account order)]
    (assert (some? (order/computed-price order)) "Order has no price; cannot send email.")
    (mailer/send
     (->mailer deps)
     (account/email account)
     (mail/subject (format "%s payment pending" (service/name service)))
     (mm/msg
      (mm/greet (account/first-name account))
      (mm/p (format "This is a friendly reminder that your monthly payment ($%.2f) for %s is being processed and should post shortly."
                    (order/computed-price order) (service/name service)))
      mail/accounting-sig)
     {:uuid (event/uuid event)
      :from mail/from-accounting})))


;; =============================================================================
;; Invoice Created
;; =============================================================================


(defmulti invoice-created ic/invoice-dispatch)


(defmethod invoice-created :default [_ event payment]
  (timbre/warn :stripe.event.invoice.created/unknown
               {:uuid         (event/uuid event)
                :invoice      (tpayment/invoice-id payment)
                :subscription (-> payment
                                  tpayment/subscription
                                  tsubscription/id)}))


(defmethod invoice-created :payment.type/rent [deps event payment]
  (let [account (-> payment tpayment/customer tcustomer/account)]
    (event/notify ::notify.rent {:params       {:account-id (td/id account)}
                                 :triggered-by event})))


(defmethod invoice-created :payment.type/order [deps event payment]
  (let [order (order/by-subscription (->db deps) (tpayment/subscription payment))]
    [(order/add-payment order payment)
     (event/notify ::notify.service {:params       {:order-id (td/id order)}
                                     :triggered-by event})]))


(defn- first-invoice?
  "Is this invoice the first invoice that's created after subscribing to a
  subscription?"
  [stripe-event]
  ;; closed upon creation means that no action needs to be taken
  (:closed (common/subject stripe-event)))

(s/fdef first-invoice?
        :args (s/cat :stripe-event map?)
        :ret boolean?)


(defmethod dispatch/stripe :stripe.event.invoice/created [deps event _]
  (let [se      (common/fetch-event (->teller deps) event)
        payment (tevent/handle-stripe-event (->teller deps) se)]
    (when-not (first-invoice? se)
      (invoice-created deps event payment))))
