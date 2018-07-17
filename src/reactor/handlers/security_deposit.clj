(ns reactor.handlers.security-deposit
  (:require [blueprints.models.account :as account]
            [blueprints.models.event :as event]
            [blueprints.models.member-license :as member-license]
            [blueprints.models.security-deposit :as deposit]
            [clj-time.coerce :as c]
            [clj-time.core :as t]
            [datomic.api :as d]
            [mailer.core :as mailer]
            [mailer.message :as mm]
            [reactor.dispatch :as dispatch]
            [reactor.handlers.common :refer :all]
            [reactor.services.slack :as slack]
            [reactor.services.slack.message :as sm]
            [reactor.utils.mail :as mail]
            [teller.payment :as tpayment]
            [toolbelt.date :as date]
            [toolbelt.datomic :as td]))

;; =============================================================================
;; First Payment
;; =============================================================================


(defmethod dispatch/report :deposit/payment-made
  [deps event {:keys [account-id payment-id]}]
  (let [account (d/entity (->db deps) account-id)
        payment (tpayment/by-id (->teller deps) payment-id)]
    (slack/send
     (->slack deps)
     {:uuid    (event/uuid event)
      :channel slack/ops}
     (sm/msg
      (sm/success
       (sm/text (format "%s has made a security deposit payment!"
                        (account/full-name account)))
       (sm/fields
        (sm/field "Amount" (str "$" (tpayment/amount payment)) true)))))))


(defmethod dispatch/job :deposit/payment-made [deps event params]
  (event/report (event/key event) {:params       params
                                   :triggered-by event}))


;; =============================================================================
;; Remainder Payment
;; =============================================================================


(defmethod dispatch/report :deposit.remainder/payment-made
  [deps event {:keys [account-id payment-id]}]
  (let [account (d/entity (->db deps) account-id)
        payment (tpayment/by-id (->teller deps) payment-id)]
    (slack/send
     (->slack deps)
     {:uuid    (event/uuid event)
      :channel slack/ops}
     (sm/msg
      (sm/success
       (sm/text (format "%s has paid the remainder of his/her security deposit"
                        (account/full-name account)))
       (sm/fields
        (sm/field "Method" "ACH" true)
        (sm/field "Amount" (format "$%.2f" (tpayment/amount payment)) true)))))))


(defmethod dispatch/job :deposit.remainder/payment-made [deps event params]
  (event/report (event/key event) {:params       params
                                   :triggered-by event}))


;; =============================================================================
;; Refund
;; =============================================================================


(defmethod dispatch/job :deposit/refund
  [deps event {:keys [deposit-id account-id]}]
  [(event/report (event/key event) {:params       {:deposit-id deposit-id
                                                   :account-id account-id}
                                    :triggered-by event})
   (event/notify (event/key event) {:params       {:deposit-id deposit-id
                                                   :account-id account-id}
                                    :triggered-by event})])


(defn- member-url [hostname account-id]
  (format "%s/accounts/%s" hostname account-id))


(defmethod dispatch/notify :deposit/refund
  [deps event {:keys [deposit-id account-id]}]
  (let [account (d/entity (->db deps) account-id)
        deposit (d/entity (->db deps) deposit-id)]
    (mailer/send
     (->mailer deps)
     (account/email account)
     (mail/subject (format "Your deposit has been refunded!"))
     (mm/msg
      (mm/greet (account/first-name account))
      (mm/p
       "We've processed your security deposit refund. You should see the money in your bank account within the next 10 days.")
      (mm/sig))
     {:uuid (event/uuid event)})))


(defmethod dispatch/report :deposit/refund
  [deps event {:keys [deposit-id account-id]}]
  (let [account (d/entity (->db deps) account-id)
        deposit (d/entity (->db deps) deposit-id)]
    (slack/send
     (->slack deps)
     {:uuid    (event/uuid event)
      :channel slack/ops}
     (sm/msg
      (sm/info
       (sm/title (str "Deposit Refunded for "
                      (account/short-name account))
                 (member-url (->dashboard-hostname deps) (td/id account)))
       (sm/fields
        (sm/field "Total" (format "%.2f" 10.0))))))))


;; =============================================================================
;; Payment Overdue
;; =============================================================================


;; =====================================
;; Internal Slack Notification


(defn- fmt-deposit [db i deposit]
  (let [account      (deposit/account deposit)
        tz           (member-license/time-zone (member-license/by-account db account))
        days-overdue (t/in-days (t/interval
                                 (date/tz-corrected-dt
                                  (c/to-date-time (deposit/due deposit)) tz)
                                 (t/now)))]
    (format "%s. %s's (_%s_) security deposit is overdue by *%s days* (_due %s_)."
            (inc i)
            (account/short-name account)
            (account/email account)
            days-overdue
            (-> deposit deposit/due (date/tz-corrected tz) (date/short true)))))


(defmethod dispatch/report :deposits/alert-unpaid
  [deps event {:keys [deposit-ids as-of]}]
  (let [deposits (apply td/entities (->db deps) deposit-ids)]
    (slack/send
     (->slack deps)
     {:uuid    (event/uuid event)
      :channel slack/ops}
     (sm/msg
      (sm/warn
       (sm/title "The following security deposits are overdue:")
       (sm/pretext "_I've gone ahead and notified each member of his/her late payment; this is just FYI._")
       (sm/text (->> deposits
                     (sort-by deposit/due)
                     (map-indexed (partial fmt-deposit (->db deps)))
                     (interpose "\n")
                     (apply str))))))))


;; =====================================
;; Email Notifications


(defn- deposit-overdue-body [db deposit hostname]
  (let [account (deposit/account deposit)
        tz      (member-license/time-zone (member-license/by-account db account))]
    (mm/msg
     (mm/greet (-> deposit deposit/account account/first-name))
     (mm/p
      (format "I hope you're settling in to the Starcity community. I'm reaching out because the remainder of your security deposit is now overdue (it was <b>due by %s</b>). Please <a href='%s/login'>log in to your account</a> to pay your balance as soon as possible."
              (date/short (date/tz-corrected (deposit/due deposit) tz) true)
              hostname))
     (mm/p "If you're having trouble remitting payment, please let me know so I can figure out how best to accommodate you.")
     mail/accounting-sig)))


(defmethod dispatch/notify :deposits/alert-unpaid
  [deps event {:keys [deposit-id]}]
  (let [deposit (d/entity (->db deps) deposit-id)]
    (mailer/send
     (->mailer deps)
     (account/email (deposit/account deposit))
     (mail/subject "Your Security Deposit is Overdue")
     (deposit-overdue-body (->db deps) deposit (->public-hostname deps))
     {:uuid (event/uuid event)
      :from mail/from-accounting})))


;; =====================================
;; Dispatch report/alerts


(defmethod dispatch/job :deposits/alert-unpaid
  [deps event {:keys [deposit-ids] :as params}]
  (conj
   (map #(event/notify (event/key event) {:params       {:deposit-id %}
                                          :triggered-by event})
        deposit-ids)
   (event/report (event/key event) {:params       params
                                    :triggered-by event})))


;; =============================================================================
;; due date upcoming
;; =============================================================================


(defn- deposit-due-soon-body [deps deposit as-of]
  (let [tz             (->> deposit
                            deposit/account
                            (member-license/active (->db deps))
                            member-license/time-zone)
        due            (date/tz-corrected (deposit/due deposit) tz)
        as-of          (date/tz-corrected as-of tz)
        days-until-due (->> due
                            c/to-date-time
                            (t/interval (c/to-date-time as-of))
                            t/in-days
                            inc)]
    (mm/msg
     (mm/greet (-> deposit deposit/account account/first-name))
     (mm/p
      (format "This is a friendly reminder to let you know that your remaining security deposit payment of $%.2f is <b>due in %s day(s)</b> by %s."
              (deposit/amount-remaining deposit) days-until-due (date/short due true)))
     (mm/p
      (format "Please <a href='%s/login'>log in to your account</a> to pay your balance as soon as possible."
              (->public-hostname deps)))
     mail/accounting-sig)))


(defmethod dispatch/notify :deposit/due [deps event {:keys [deposit-id as-of]}]
  (let [deposit (d/entity (->db deps) deposit-id)]
    (mailer/send
     (->mailer deps)
     (account/email (deposit/account deposit))
     (mail/subject "Your Security Deposit is Due Soon")
     (deposit-due-soon-body deps deposit as-of)
     {:uuid (event/uuid event)
      :from mail/from-accounting})))


(comment

  (t/in-days (t/interval (t/now) (t/plus (t/now) (t/days 5))))

  )
