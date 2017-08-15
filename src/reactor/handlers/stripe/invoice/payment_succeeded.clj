(ns reactor.handlers.stripe.invoice.payment-succeeded
  (:require [blueprints.models.account :as account]
            [blueprints.models.event :as event]
            [blueprints.models.member-license :as member-license]
            [blueprints.models.payment :as payment]
            [blueprints.models.rent-payment :as rent-payment]
            [mailer.core :as mailer]
            [mailer.message :as mm]
            [reactor.dispatch :as dispatch]
            [reactor.handlers.common :refer :all]
            [reactor.handlers.stripe.common :as common]
            [reactor.handlers.stripe.invoice.common :as ic]
            [reactor.services.slack :as slack]
            [reactor.services.slack.message :as sm]
            [ribbon.event :as re]
            [taoensso.timbre :as timbre]))

;; =============================================================================
;; Notify
;; =============================================================================


(defmethod dispatch/notify ::notify.rent [deps event {:keys [invoice]}]
  (let [license (member-license/by-invoice-id (->db deps) invoice)
        account (member-license/account license)
        payment (rent-payment/by-invoice-id (->db deps) invoice)]
    (mailer/send
     (->mailer deps)
     (account/email account)
     "Starcity: Autopay Payment Successful"
     (mm/msg
      (mm/greet (account/first-name account))
      (mm/p
       (format "This is a friendly reminder to let you know that your rent payment of $%.2f has been successfully paid."
               (rent-payment/amount payment)))
      (mm/p "Thanks for using Autopay!")
      (mm/sig))
     {:uuid (event/uuid event)})))


;; =============================================================================
;; Report
;; =============================================================================


(defmethod dispatch/report ::notify.rent [deps event {:keys [invoice]}]
  (let [license (member-license/by-invoice-id (->db deps) invoice)
        account (member-license/account license)
        managed (member-license/managed-account-id license)]
    (slack/send
     (->slack deps)
     {:channel slack/ops
      :uuid    (event/uuid event)}
     (sm/msg
      (sm/success
       (sm/title "View Invoice on Stripe" (ic/invoice-dashboard-url managed invoice))
       (sm/text (format "%s's autopay payment has succeeded!"
                        (account/full-name account))))))))


;; =============================================================================
;; Payment Succeeded
;; =============================================================================


(defmulti payment-succeeded ic/invoice-dispatch)


(defmethod payment-succeeded :default [_ event stripe-event]
  (timbre/warn :stripe.event.invoice.payment-succeeded/unknown
               {:uuid         (event/uuid event)
                :invoice      (re/subject-id stripe-event)
                :subscription (ic/subs-id stripe-event)}))


(defmethod payment-succeeded :rent [deps event stripe-event]
  (let [payment (rent-payment/by-invoice-id (->db deps) (re/subject-id stripe-event))]
    (-> (mapv
         (fn [topic]
           (let [params {:invoice (re/subject-id stripe-event)}]
             (event/create ::notify.rent {:params       params
                                          :triggered-by event
                                          :topic        topic})))
         [:report :notify])
        (conj (rent-payment/set-paid payment)))))


(defmethod payment-succeeded :service [deps event stripe-event]
  (let [invoice-id (re/subject-id stripe-event)
        payment    (payment/by-invoice-id (->db deps) invoice-id)]
    ;; No notifications required at this time.
    (payment/is-paid payment)))


(defn- linked-invoice? [deps stripe-event]
  (let [invoice (re/subject-id stripe-event)]
    (or (rent-payment/by-invoice-id (->db deps) invoice)
        (try (payment/by-invoice-id (->db deps) invoice)
             (catch Throwable _ nil)))))


(defmethod dispatch/stripe :stripe.event.invoice/payment-succeeded
  [deps event stripe-event]
  (let [stripe-event (common/fetch-event (->stripe deps) event)]
    (when (linked-invoice? deps stripe-event)
      (payment-succeeded deps event stripe-event))))
