(ns reactor.handlers.stripe.invoice
  (:require [blueprints.models
             [member-license :as member-license]
             [order :as order]
             [payment :as payment]]
            [clj-time.coerce :as c]
            [reactor.dispatch :as dispatch]
            [reactor.handlers.common :refer :all]
            [reactor.handlers.stripe.common :as common]
            [reactor.models.event :as event]
            [ribbon
             [core :as ribbon]
             [event :as re]]
            [taoensso.timbre :as timbre]
            [toolbelt.date :as date]
            [blueprints.models.rent-payment :as rent-payment]
            [toolbelt.datomic :as td]
            [mailer.core :as mailer]
            [datomic.api :as d]
            [blueprints.models.account :as account]
            [mailer.message :as mm]
            [clojure.spec :as s]
            [blueprints.models.service :as service]
            [blueprints.models.charge :as charge]))

;; =============================================================================
;; Helpers
;; =============================================================================


(defn subs-id
  "The id of the subscription that this invoice is for."
  [stripe-event]
  (-> stripe-event re/subject :subscription))


(defn period-start
  "Produce the invoice's start date by inspecting the line items. The
  `:period_start` date on the `event-data` has proven to be unreliable; hence
  the use of the `:lines`."
  [license event-data]
  (-> event-data re/subject :lines :data first :period :start (* 1000) c/from-long c/to-date))


(defn invoice-type
  "Given a `stripe-event`, determine what kind of invoice it's attached to."
  [db stripe-event]
  (let [subs-id (subs-id stripe-event)]
    (cond
      (some? (member-license/by-subscription-id db subs-id))
      :rent

      (some? (order/by-subscription-id db subs-id))
      :service

      :otherwise :unknown)))


(defn invoice-dispatch [deps event stripe-event]
  (invoice-type (->db deps) stripe-event))

(comment
  (def stripe
    (ribbon/stripe-connection (reactor.config/stripe-secret-key reactor.config/config)))


  (common/fetch-event stripe {:event/id   "evt_1AQ21HJDow24Tc1auKJnmQFA"
                              :event/meta (pr-str {:managed-account "acct_191838JDow24Tc1a"})})


  )


;; =============================================================================
;; Created
;; =============================================================================


;; Invoices are created ~1 hour prior to the attempt to charge them. We use this
;; event to ready the system for the charge.


(defmethod dispatch/mail :stripe.event.invoice.created.rent/send-email
  [deps event {:keys [account-id]}]
  (let [account (d/entity (->db deps) account-id)]
    (mailer/send
     (->mailer deps)
     (account/email account)
     "Starcity: Autopay Payment Pending"
     (mm/msg
      (mm/greet (account/first-name account))
      (mm/p "This is a friendly reminder that your autopay payment is being processed.")
      (mm/p "Please note that it may take up to <b>7 business days</b> for the funds to be withdrawn from your account.")
      (mm/sig))
     {:uuid (event/uuid event)})))


(defmethod dispatch/mail :stripe.event.invoice.created.service/send-email
  [deps event {:keys [subs-id]}]
  (let [order   (order/by-subscription-id (->db deps) subs-id)
        service (order/service order)
        account (order/account order)]
    (assert (some? (order/price order)) "Order has no price; cannot send email.")
    (mailer/send
     (->mailer deps)
     (account/email account)
     (format "Starcity: %s payment pending" (service/name service))
     (mm/msg
      (mm/greet (account/first-name account))
      (mm/p (format "This is a friendly reminder that your monthly payment ($%.2f) for %s is being processed and should post shortly."
                    (order/price order) (service/name service)))
      (mm/sig))
     {:uuid (event/uuid event)})))


(defmulti invoice-created invoice-dispatch)


(defmethod invoice-created :default [_ event stripe-event]
  (timbre/warn :stripe.event.invoice.created/unknown
               {:uuid         (event/uuid event)
                :invoice      (re/subject-id stripe-event)
                :subscription (subs-id stripe-event)}))


(defmethod invoice-created :rent [deps event stripe-event]
  (let [license (member-license/by-subscription-id (->db deps) (subs-id stripe-event))
        pstart  (period-start license stripe-event)
        payment (rent-payment/autopay-payment license (re/subject-id stripe-event) pstart)]
    [(member-license/add-rent-payments license payment)
     (event/create :stripe.event.invoice.created.rent/send-email
                   {:params       {:account-id (td/id (member-license/account license))}
                    :topic        :mail
                    :triggered-by event})]))


(defmethod invoice-created :service [deps event stripe-event]
  (let [order   (order/by-subscription-id (->db deps) (subs-id stripe-event))
        payment (payment/create (order/price order) :for :payment.for/order)]
    [(order/add-payment order payment)
     (merge payment (payment/add-invoice payment (re/subject-id stripe-event)))
     (event/create :stripe.event.invoice.created.service/send-email
                   {:params       {:subs-id (subs-id stripe-event)}
                    :topic        :mail
                    :triggered-by event})]))


(defn- first-invoice?
  "Is this invoice the first invoice that's created after subscribing to a
  subscription?"
  [stripe-event]
  ;; closed upon creation means that no action needs to be taken
  (:closed (re/subject stripe-event)))

(s/fdef first-invoice?
        :args (s/cat :stripe-event map?)
        :ret boolean?)


(defmethod dispatch/stripe :stripe.event.invoice/created [deps event _]
  (let [stripe-event (common/fetch-event (->stripe deps) event)]
    (when-not (first-invoice? stripe-event)
      (invoice-created deps event stripe-event))))


;; =============================================================================
;; Updated
;; =============================================================================


;; The "invoice.updated" event is issued after Stripe has attempted to charge
;; the customer. It's at this point that we'll first have a reference to a
;; Stripe charge id.


(defmulti invoice-updated invoice-dispatch)


(defmethod invoice-updated :default [_ event stripe-event]
  (timbre/warn :stripe.event.invoice.updated/unknown
               {:uuid         (event/uuid event)
                :invoice      (re/subject-id stripe-event)
                :subscription (subs-id stripe-event)}))


(defmethod invoice-updated :rent [deps event stripe-event]
  (let [invoice-id (re/subject-id stripe-event)
        license    (member-license/by-invoice-id (->db deps) invoice-id)
        payment    (rent-payment/by-invoice-id (->db deps) invoice-id)
        charge-id  (:charge (re/subject stripe-event))]
    (when-not (some? (rent-payment/charge payment))
      (assert (some? charge-id) "event has no charge id")
      {:db/id               (td/id payment)
       :rent-payment/charge (charge/create (member-license/account license)
                                           charge-id
                                           (rent-payment/amount payment))})))


(defmethod invoice-updated :service [deps event stripe-event]
  (let [invoice-id (re/subject-id stripe-event)
        payment    (payment/by-invoice-id (->db deps) invoice-id)]
    (payment/add-charge payment (:charge (re/subject stripe-event)))))


(defmethod dispatch/stripe :stripe.event.invoice/updated [deps event _]
  (let [stripe-event (common/fetch-event (->stripe deps) event)]
    (invoice-updated deps event stripe-event)))


;; =============================================================================
;; Payment Succeeded
;; =============================================================================


(defmulti payment-succeeded invoice-dispatch)


(defmethod payment-succeeded :default [_ event stripe-event]
  (timbre/warn :stripe.event.invoice.payment-succeeded/unknown
               {:uuid         (event/uuid event)
                :invoice      (re/subject-id stripe-event)
                :subscription (subs-id stripe-event)}))


(defmethod payment-succeeded :rent [deps event stripe-event]
  (let [payment (rent-payment/by-invoice-id (->db deps) (re/subject-id stripe-event))]
    [(rent-payment/set-paid payment)
     (event/create :stripe.event.invoice.payment-succeeded.rent/send-email
                   {:params       {:invoice (re/subject-id stripe-event)}
                    :topic        :mail
                    :triggered-by event})
     (event/create :stripe.event.invoice.payment-succeeded.rent/send-slack
                   {:params       {:invoice (re/subject-id stripe-event)}
                    :topic        :slack
                    :triggered-by event})]))


(defmethod dispatch/stripe :stripe.event.invoice/payment-succeeded
  [deps event stripe-event]
  (let [stripe-event (common/fetch-event (->stripe deps) event)]
    (payment-succeeded deps event stripe-event)))
