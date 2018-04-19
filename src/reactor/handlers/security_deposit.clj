(ns reactor.handlers.security-deposit
  (:require [blueprints.models.account :as account]
            [blueprints.models.event :as event]
            [blueprints.models.member-license :as member-license]
            [blueprints.models.payment :as payment]
            [blueprints.models.security-deposit :as deposit]
            [clj-time.coerce :as c]
            [clj-time.core :as t]
            [clojure.spec.alpha :as s]
            [clojure.string :as string]
            [datomic.api :as d]
            [mailer.core :as mailer]
            [mailer.message :as mm]
            [reactor.dispatch :as dispatch]
            [reactor.handlers.common :refer :all]
            [reactor.services.slack :as slack]
            [reactor.services.slack.message :as sm]
            [reactor.utils.mail :as mail]
            [ribbon.charge :as rc]
            [taoensso.timbre :as timbre]
            [teller.payment :as tpayment]
            [toolbelt.async :as ta :refer [<!!?]]
            [toolbelt.core :as tb]
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
  [deps event {:keys [account-id charge]}]
  (let [account (d/entity (->db deps) account-id)
        payment (payment/by-charge-id (->teller deps) charge)]
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
        (sm/field "Amount" (format "$%.2f" (payment/amount payment)) true)))))))


(defmethod dispatch/job :deposit.remainder/payment-made [deps event params]
  (event/report (event/key event) {:params       params
                                   :triggered-by event}))


;; =============================================================================
;; Refund
;; =============================================================================


(defmethod dispatch/notify :deposit.refund/finish
  [deps event {:keys [deposit-id amount reasons]}]
  (let [deposit (d/entity (->db deps) deposit-id)
        account (deposit/account deposit)
        partial (< amount (deposit/amount deposit))]
    (mailer/send
     (->mailer deps)
     (account/email account)
     (mail/subject "Security Deposit Refund")
     (mm/msg
      (mm/greet (account/first-name account))
      (if partial
        (mm/p (format "We've refunded <b>$%.2f</b> of your <b>$%.2f</b> deposit for the following reasons:"
                      amount (deposit/amount deposit)))
        (mm/p (format "Your entire deposit of <b>$%.2f</b> has been refunded."
                      (deposit/amount deposit))))
      (when partial
        (mm/p (format "<i>%s</i>" (-> reasons (string/replace #"\n" "<br>")))))
      (mm/p "It may take up to <b>10 business days</b> for the funds to be returned to your account.")
      mail/accounting-sig)
     {:uuid (event/uuid event)
      :from mail/from-accounting})))


(defmethod dispatch/job :deposit.refund/finish
  [deps event {:keys [deposit-id amount reasons] :as params}]
  [(event/notify (event/key event) {:params       params
                                    :triggered-by event})
   {:db/id                 deposit-id
    :deposit/refund-status :deposit.refund-status/successful}])


;; NOTE: This isn't needed! Since security deposit payments are made via the
;; platform account, we don't need to pass the managed account id.
;; (defn- managed-account
;;   "Determine the id of the connected account that `payment` was made with."
;;   [deps payment]
;;   (let [account (payment/account payment)
;;         license (member-license/active (->db deps) account)]
;;     (try
;;       (timbre/debug ::check-deposit-account {:payment-id (:db/id payment)})
;;       (rc/fetch (->stripe deps) (payment/charge-id payment)
;;                 :managed-account (member-license/deposit-connect-id license))
;;       (member-license/rent-connect-id license)
;;       (catch Throwable _
;;         (timbre/debug ::check-rent-account {:payment-id (:db/id payment)})
;;         (rc/fetch (->stripe deps) (payment/charge-id payment)
;;                   :managed-account (member-license/rent-connect-id license))
;;         (member-license/deposit-connect-id license)))))

;; (s/fdef managed-account
;;         :args (s/cat :deps map? :payment td/entity?)
;;         :ret string?)


(defn refund-payment!
  "Refund `amount` from `payment`."
  [deps payment amount]
  (<!!? (rc/refund! (->stripe deps) (payment/charge-id payment)
                    :amount (float amount))))

(s/fdef refund-payment!
        :args (s/cat :deps map? :payment td/entity? :amount number?))


(defmethod dispatch/job :deposit.refund/payment
  [deps event {:keys [deposit-id payment-id refund-amount remaining] :as params}]
  (let [payment (d/entity (->db deps) payment-id)]
    (try
      (refund-payment! deps payment refund-amount)
      (if-some [{:keys [payment-id refund-amount]} (first remaining)]
        (event/job :deposit.refund/payment
                   {:params       (merge
                                   {:deposit-id    deposit-id
                                    :payment-id    payment-id
                                    :refund-amount refund-amount
                                    :remaining     (rest remaining)}
                                   params)
                    :triggered-by event})
        (event/job :deposit.refund/finish
                   {:params       (select-keys params [:deposit-id :amount :reasons])
                    :triggered-by event}))
      (catch Throwable t
        (timbre/error t :deposit.refund/payment {:deposit-id deposit-id
                                                 :payment-id payment-id})
        (throw (ex-info "Error encountered while attempting to refund payment!"
                        {:deposit-id deposit-id
                         :payment-id payment-id
                         :tx         [{:db/id                 deposit-id
                                       :deposit/refund-status :deposit.refund-status/failed}]}))))))


(defn- refund-params
  "Look through `deposit`'s payments and determine how much, if any, to deduct
  from each one. This function produces part of the parameters needed for the
  `:deposit.refund/payment` event."
  [deposit refund]
  (-> (reduce
       (fn [[total py-params] payment]
         (cond
           (zero? total)
           [total py-params]

           (< total (payment/amount payment))
           [0
            (conj py-params {:payment-id    (:db/id payment)
                             :refund-amount total})]

           :otherwise
           [(- total (payment/amount payment))
            (conj py-params {:payment-id    (:db/id payment)
                             :refund-amount (payment/amount payment)})]))
       [refund []]
       (sort-by (comp min payment/amount) (deposit/payments deposit)))
      (second)))


(s/def ::payment-id integer?)
(s/def ::refund-amount (s/and float? pos?))
(s/fdef refund-params
        :args (s/cat :deposit td/entity?
                     :refund (s/and float? pos?))
        :ret (s/+ (s/keys :req-un [::payment-id ::refund-amount])))


(defn refund-deposit [deps event deposit amount reasons]
  (let [py-params (refund-params deposit amount)
        remaining (when (> (count py-params) 1) (rest py-params))]
    (event/job :deposit.refund/payment {:params       (merge
                                                       (first py-params)
                                                       (tb/assoc-when
                                                        {:deposit-id (:db/id deposit)
                                                         :amount     amount
                                                         :reasons    reasons}
                                                        :remaining remaining))
                                        :triggered-by event})))

(s/fdef refund-deposit
        :args (s/cat :deps map?
                     :event td/entity?
                     :deposit td/entity?
                     :amount float?
                     :reasons string?)
        :ret map?)


(defmethod dispatch/job :deposit/refund
  [deps event {:keys [deposit-id amount reasons]}]
  (let [deposit (d/entity (->db deps) deposit-id)]
    (assert (#{:deposit.refund-status/initiated} (deposit/refund-status deposit))
            "The deposit must be `:deposit.refund-status/initiated`.")
    (assert (every? payment/charge? (deposit/payments deposit))
            "Cannot refund deposit made without Stripe payments.")
    (refund-deposit deps event deposit (float amount) reasons)))



(comment

  (let [deposit {:db/id            1234
                 :deposit/amount   2300.0
                 :deposit/payments [{:db/id          8619045951
                                     :payment/amount 1800.0}
                                    {:db/id          1910691485
                                     :payment/amount 500.0}]}
        amount  2250.0]
    (refund-params deposit amount))



  )


;; =============================================================================
;; Payment Overdue
;; =============================================================================


;; =====================================
;; Internal Slack Notification


(defn- fmt-deposit [db i deposit]
  (let [account      (deposit/account deposit)
        tz           (member-license/time-zone (member-license/by-account db account))
        days-overdue (t/in-days (t/interval
                                 (date/to-utc-corrected-date-time
                                  (c/to-date-time (deposit/due deposit)) tz)
                                 (t/now)))]
    (format "%s. %s's (_%s_) security deposit is overdue by *%s days* (_due %s_)."
            (inc i)
            (account/short-name account)
            (account/email account)
            days-overdue
            (-> deposit deposit/due (date/to-utc-corrected-date tz) date/short-date-time))))


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
              (date/short-date-time (date/to-utc-corrected-date (deposit/due deposit) tz))
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
        due            (date/to-utc-corrected-date (deposit/due deposit) tz)
        as-of          (date/to-utc-corrected-date as-of tz)
        days-until-due (->> due
                            c/to-date-time
                            (t/interval (c/to-date-time as-of))
                            t/in-days
                            inc)]
    (mm/msg
     (mm/greet (-> deposit deposit/account account/first-name))
     (mm/p
      (format "This is a friendly reminder to let you know that your remaining security deposit payment of $%.2f is <b>due in %s day(s)</b> by %s." (deposit/amount-remaining deposit) days-until-due (date/short-date-time due)))
     (mm/p
      (format "Please <a href='%s/login'>log in to your account</a> to pay your balance as soon as possible." (->public-hostname deps)))
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
