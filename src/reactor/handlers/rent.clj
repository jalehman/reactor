(ns reactor.handlers.rent
  (:require [blueprints.models.account :as account]
            [blueprints.models.event :as event]
            [blueprints.models.member-license :as member-license]
            [blueprints.models.payment :as payment]
            [datomic.api :as d]
            [mailer.core :as mailer]
            [mailer.message :as mm]
            [reactor.dispatch :as dispatch]
            [reactor.handlers.common :refer :all]
            [reactor.services.slack :as slack]
            [reactor.services.slack.message :as sm]
            [toolbelt.date :as date]
            [toolbelt.datomic :as td]))

;; =============================================================================
;; Create Payment
;; =============================================================================


(defn rent-reminder-body [account amount hostname]
  (mm/msg
   (mm/greet (account/first-name account))
   (mm/p (format "It's that time again! Your rent payment of $%.2f is <b>due by the 5th</b>." amount))
   (mm/p "Please log into your member dashboard " [:a {:href (str hostname "/me/account/rent")} "here"]
         " to pay your rent with ACH. <b>If you'd like to stop getting these reminders, sign up for autopay while you're there!</b>")
   (mm/sig)))


(defmethod dispatch/notify :rent-payment/create
  [deps event {:keys [member-license-id amount]}]
  (let [license (d/entity (->db deps) member-license-id)
        account (member-license/account license)]
    (mailer/send
     (->mailer deps)
     (account/email account)
     "Starcity: Your Rent is Due"
     (rent-reminder-body account amount (->public-hostname deps))
     {:uuid (event/uuid event)})))


(defmethod dispatch/job :rent-payment/create
  [deps event {:keys [start end amount member-license-id] :as params}]
  (let [license (d/entity (->db deps) member-license-id)
        account (member-license/account license)
        payment (payment/create amount account
                                :pstart start
                                :pend end
                                :status :payment.status/due
                                :for :payment.for/rent)]
    [(event/notify :rent-payment/create
                   {:params       {:member-license-id member-license-id
                                   :amount            amount}
                    :triggered-by event})
     {:db/id                        member-license-id
      :member-license/rent-payments payment}]))


;; =============================================================================
;; Create All Payments
;; =============================================================================


;; The `:rent-payments/create-all` should be triggered by a scheduler on the
;; first of the month. This event then spawns a new event for each member that
;; needs to have a rent payment generated for him/her.


(defn active-licenses
  "Query all active licenses that are not on autopay that have not yet commenced."
  [db period]
  (d/q '[:find ?l ?p
         :in $ ?period
         :where
         ;; active licenses
         [?l :member-license/status :member-license.status/active]
         [?l :member-license/unit ?u]
         [?l :member-license/price ?p]
         [?l :member-license/commencement ?c]
         [?l :member-license/rent-payments ?py]
         ;; not on autopay
         [(missing? $ ?l :member-license/subscription-id)]
         ;; license has commenced
         [(.before ^java.util.Date ?c ?period)] ; now is after commencement
         ]
       db period))


(defn create-payment-events
  [db event period]
  (let [actives (active-licenses db period)]
    (->> (mapv
          (fn [[member-license-id amount]]
            (let [ml    (d/entity db member-license-id)
                  tz    (member-license/time-zone ml)
                  start (date/beginning-of-day period tz)
                  end   (date/end-of-month start tz)]
              (when (empty? (member-license/payment-within db ml period))
                (event/job :rent-payment/create {:params       {:start             start
                                                                :end               end
                                                                :amount            amount
                                                                :member-license-id member-license-id}
                                                 :triggered-by event}))))
          actives)
         (remove nil?))))


(defmethod dispatch/job :rent-payments/create-all [deps event params]
  (assert (:period params) "The time period to create payments for must be supplied!")
  (create-payment-events (->db deps) event (:period params)))


;; =============================================================================
;; ACH Rent Payment Made
;; =============================================================================


(defmethod dispatch/report :rent-payment.payment/ach
  [deps event {:keys [account-id payment-id]}]
  (let [[account payment] (td/entities (->db deps) account-id payment-id)]
    (slack/send
     (->slack deps)
     {:uuid    (event/uuid event)
      :channel slack/ops}
     (sm/msg
      (sm/success
       (sm/title "View Payment on Stripe"
                 (format "https://dashboard.stripe.com/payments/%s" (payment/charge-id payment)))
       (sm/text (format "%s has paid his/her rent via ACH" (account/full-name account)))
       (sm/fields
        (sm/field "Amount"
                  (str "$" (payment/amount payment))
                  true)
        (sm/field "Period Start"
                  (date/short-date (payment/period-start payment))
                  true)
        (sm/field "Period End"
                  (date/short-date (payment/period-end payment))
                  true)))))))


(defmethod dispatch/job :rent-payment.payment/ach [deps event params]
  (event/report (event/key event) {:params       params
                                   :triggered-by event}))
