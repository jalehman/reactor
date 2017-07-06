(ns reactor.handlers.stripe.invoice.created
  (:require [blueprints.models.account :as account]
            [blueprints.models.member-license :as member-license]
            [blueprints.models.order :as order]
            [blueprints.models.payment :as payment]
            [blueprints.models.rent-payment :as rent-payment]
            [blueprints.models.service :as service]
            [clj-time.coerce :as c]
            [clojure.spec :as s]
            [datomic.api :as d]
            [mailer.core :as mailer]
            [mailer.message :as mm]
            [reactor.dispatch :as dispatch]
            [reactor.handlers.common :refer :all]
            [reactor.handlers.stripe.common :as common]
            [reactor.handlers.stripe.invoice.common :as ic]
            [reactor.models.event :as event]
            [ribbon.event :as re]
            [taoensso.timbre :as timbre]
            [toolbelt.datomic :as td]))

;; Invoices are created ~1 hour prior to the attempt to charge them. We use this
;; event to ready the system for the charge.


;; =============================================================================
;; Notify
;; =============================================================================


(defmethod dispatch/notify ::notify.rent [deps event {:keys [account-id]}]
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


(defmethod dispatch/notify ::notify.service [deps event {:keys [subs-id]}]
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


;; =============================================================================
;; Invoice Created
;; =============================================================================


(defn period-start
  "Produce the invoice's start date by inspecting the line items. The
  `:period_start` date on the `event-data` has proven to be unreliable; hence
  the use of the `:lines`."
  [license event-data]
  (-> event-data re/subject :lines :data first :period :start (* 1000) c/from-long c/to-date))


(defmulti invoice-created ic/invoice-dispatch)


(defmethod invoice-created :default [_ event stripe-event]
  (timbre/warn :stripe.event.invoice.created/unknown
               {:uuid         (event/uuid event)
                :invoice      (re/subject-id stripe-event)
                :subscription (ic/subs-id stripe-event)}))


(defmethod invoice-created :rent [deps event stripe-event]
  (let [license (member-license/by-subscription-id (->db deps) (ic/subs-id stripe-event))
        pstart  (period-start license stripe-event)
        payment (rent-payment/autopay-payment license (re/subject-id stripe-event) pstart)
        account (member-license/account license)]
    [(member-license/add-rent-payments license payment)
     (event/notify ::notify.rent {:params       {:account-id (td/id account)}
                                  :triggered-by event})]))


(defmethod invoice-created :service [deps event stripe-event]
  (let [order   (order/by-subscription-id (->db deps) (ic/subs-id stripe-event))
        payment (payment/create (order/price order) :for :payment.for/order)]
    [(order/add-payment order payment)
     (merge payment (payment/add-invoice payment (re/subject-id stripe-event)))
     (event/notify ::notify.service {:params       {:subs-id (ic/subs-id stripe-event)}
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
