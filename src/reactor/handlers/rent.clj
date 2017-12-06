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
            [toolbelt.datomic :as td]
            [clj-time.coerce :as c]
            [clj-time.core :as t]
            [taoensso.timbre :as timbre]))

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


(defn- due-date [start tz]
  (let [st (c/to-date-time start)]
    (-> (t/date-time (t/year st)
                     (t/month st)
                     5
                     (t/hour st)
                     (t/minute st)
                     (t/second st))
        c/to-date
        (date/end-of-day tz))))


(defmethod dispatch/job :rent-payment/create
  [deps event {:keys [start end amount member-license-id] :as params}]
  (let [license (d/entity (->db deps) member-license-id)
        account (member-license/account license)
        payment (payment/create amount account
                                :pstart start
                                :pend end
                                :due (due-date start (member-license/time-zone license))
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
;; Alert Unpaid Payments
;; =============================================================================


(defn- payment-period [payment tz]
  (str (date/short-date (date/from-tz-date (payment/period-start payment) tz))
       "-"
       (date/short-date (date/from-tz-date (payment/period-end payment) tz))))


;; =====================================
;; Internal Slack notification


(defn- fmt-payment [db i payment]
  (let [account      (payment/account payment)
        tz           (member-license/time-zone (member-license/by-account db account))
        days-overdue (t/in-days (t/interval
                                 (date/from-tz-date-time (c/to-date-time (payment/period-start payment)) tz)
                                 (t/now)))]
    (format "%s. %s's (_%s_) rent for `%s` is overdue by *%s days* (_due %s_), and late fees will be assessed."
            (inc i)
            (account/short-name account)
            (account/email account)
            (payment-period payment tz)
            days-overdue
            (-> payment payment/due (date/from-tz-date tz) date/short-date-time))))


(defmethod dispatch/report :rent-payments/alert-unpaid
  [deps event {:keys [payment-ids as-of]}]
  (let [payments (apply td/entities (->db deps) payment-ids)]
    (slack/send
     (->slack deps)
     {:uuid    (event/uuid event)
      :channel slack/ops}
     (sm/msg
      (sm/warn
       (sm/title "The following rent payments are overdue:")
       (sm/pretext "_I've gone ahead and notified each member of his/her late payment; this is just FYI._")
       (sm/text (->> payments
                     (sort-by payment/due)
                     (map-indexed (partial fmt-payment (->db deps)))
                     (interpose "\n")
                     (apply str))))))))


;; =====================================
;; Member email


(defn- rent-overdue-body [db payment hostname]
  (let [account (payment/account payment)
        tz      (member-license/time-zone (member-license/by-account db account))]
    (mm/msg
     (mm/greet (-> payment payment/account account/first-name))
     (mm/p
      (format "I hope all is well. I wanted to check in because your <b>rent for %s is now overdue and past the grace period</b> (the grace period ended on %s). Please <a href='%s/login'>log in to your account</a> to pay your balance at your earliest opportunity."
              (payment-period payment tz)
              (date/short-date-time (date/from-tz-date-time (payment/due payment) tz))
              hostname))
     (mm/p "While you're there, I'd highly encourage you to enroll in <b>Autopay</b> so you don't have to worry about missing due dates and having late fees assessed in the future.")
     (mm/p "If you're having trouble remitting payment, please let us know so we can figure out how best to accommodate you.")
     (mm/sig "Meagan Jensen" "Operations Associate"))))


(defmethod dispatch/notify :rent-payments/alert-unpaid
  [deps event {:keys [payment-id]}]
  (let [payment (d/entity (->db deps) payment-id)]
    (mailer/send
     (->mailer deps)
     (account/email (payment/account payment))
     "Starcity: Your Rent is Overdue"
     (rent-overdue-body (->db deps) payment (->public-hostname deps))
     {:uuid (event/uuid event)
      :from "Starcity <meagan@joinstarcity.com>"})))


;; =====================================
;; Dispatch report/notify events


(defn- rent-payment? [db payment]
  (= (payment/payment-for2 db payment) :payment.for/rent))


(defmethod dispatch/job :rent-payments/alert-unpaid
  [deps event {:keys [payment-ids as-of] :as params}]
  (let [payments (apply td/entities (->db deps) payment-ids)]
    (assert (every? (partial rent-payment? (->db deps)) payments)
            "All payments must be rent payments; not processing.")
    (conj
     ;; notify each member
     (map #(event/notify (event/key event) {:params       {:payment-id (td/id %)}
                                            :triggered-by event})
          payments)
     (event/report (event/key event) {:params       params
                                      :triggered-by event}))))


;; =============================================================================
;; due date upcoming
;; =============================================================================


;; it may make sense to move this to a `payment` namespace when we're dealing
;; with multiple kinds of payments.

(defn- payment-due-soon-body [deps payment as-of]
  (let [tz  (->> payment
                 payment/account
                 (member-license/active (->db deps))
                 member-license/time-zone)
        due (date/from-tz-date (payment/due payment) tz)]
    (mm/msg
     (mm/greet (-> payment payment/account account/first-name))
     (mm/p
      (format "This is a friendly reminder to let you know that your rent payment of $%.2f <b>must be made by %s</b> to avoid late fees." (payment/amount payment) (date/short-date-time due)))
     (mm/p
      (format "Please <a href='%s/login'>log in to your account</a> to pay your rent as soon as possible." (->public-hostname deps)))
     (mm/sig "Meagan Jensen" "Operations Associate"))))


(defmethod dispatch/notify :payment/due [deps event {:keys [payment-id as-of]}]
  (let [payment (d/entity (->db deps) payment-id)]
    (assert (= (payment/payment-for2 (->db deps) payment) :payment.for/rent)
            "Can only work with rent payments; not processing.")
    (mailer/send
     (->mailer deps)
     (account/email (payment/account payment))
     "Starcity: Your Rent is Due Soon"
     (payment-due-soon-body deps payment as-of)
     {:uuid (event/uuid event)
      :from "Starcity <meagan@joinstarcity.com>"})))
