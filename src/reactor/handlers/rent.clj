(ns reactor.handlers.rent
  (:require [blueprints.models.account :as account]
            [blueprints.models.event :as event]
            [blueprints.models.member-license :as member-license]
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
            [teller.customer :as tcustomer]
            [teller.payment :as tpayment]
            [teller.property :as tproperty]
            [teller.subscription :as tsubscription]
            [toolbelt.date :as date]
            [toolbelt.datomic :as td]))

;; =============================================================================
;; Create Payment
;; =============================================================================


(defn rent-reminder-body [account amount hostname]
  (mm/msg
   (mm/greet (account/first-name account))
   (mm/p (format "It's that time again! Your rent payment of $%.2f is <b>due by the 5th</b>." amount))
   (mm/p "Please log into your member dashboard " [:a {:href (str hostname "/profile")} "here"]
         " to pay your rent with ACH. <b>If you'd like to stop getting these reminders, sign up for autopay while you're there!</b>")
   mail/accounting-sig))


(defmethod dispatch/notify :rent-payment/create
  [deps event {:keys [member-license-id amount]}]
  (let [license (d/entity (->db deps) member-license-id)
        account (member-license/account license)]
    (mailer/send
     (->mailer deps)
     (account/email account)
     (mail/subject "Your Rent is Due")
     (rent-reminder-body account amount (->dashboard-hostname deps))
     {:uuid (event/uuid event)
      :from mail/accounting-sig})))


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
  (let [license   (d/entity (->db deps) member-license-id)
        account   (member-license/account license)
        community (account/current-property (->db deps) account)
        property  (tproperty/by-community (->teller deps) community)
        customer  (tcustomer/by-account (->teller deps) account)
        due       (due-date start (member-license/time-zone license))
        payment   (tpayment/create! customer amount :payment.type/rent
                                    {:property property
                                     :due      due
                                     :status   :payment.status/due
                                     :period   [start end]})]
    [(event/notify :rent-payment/create
                   {:params       {:member-license-id member-license-id
                                   :amount            amount}
                    :triggered-by event})]))


;; =============================================================================
;; Create All Payments
;; =============================================================================


;; The `:rent-payments/create-all` should be triggered by a scheduler on the
;; first of the month. This event then spawns a new event for each member that
;; needs to have a rent payment generated for him/her.


(defn active-licenses
  "Query all active licenses that have not yet commenced."
  [db period]
  (d/q '[:find ?l ?p
         :in $ ?period
         :where
         ;; active licenses
         [?l :member-license/status :member-license.status/active]
         [?l :member-license/unit ?u]
         [?l :member-license/price ?p]
         [?l :member-license/commencement ?c]
         ;; license has commenced
         [(.before ^java.util.Date ?c ?period)]]
       db period))


(defn- on-autopay?
  [teller account]
  (when-let [customer (tcustomer/by-account teller account)]
    (->> (tsubscription/query teller {:customers     [customer]
                                      :payment-types [:payment.type/rent]})
         (filter tsubscription/active?)
         seq)))


(defn- has-current-rent-payment?
  [teller account from]
  (when-let [customer (tcustomer/by-account teller account)]
    (empty?
     (tpayment/query teller {:customers     [customer]
                             :payment-types [:payment.type/rent]
                             :from          (c/to-date (t/minus from (t/days 1)))
                             :to            (c/to-date (t/plus from (t/days 1)))
                             :datekey       :payment/pstart}))))


(defn- should-create-rent-payment?
  [teller account from]
  (let [from (c/to-date-time from)]
    (and
     (not (on-autopay? teller account))
     (has-current-rent-payment? teller account from))))


(defn- payment-end-date
  [license start]
  (let [tz  (member-license/time-zone license)
        end (date/end-of-month start tz)]
    (if (t/within? (t/interval (c/to-date-time start) (c/to-date-time end))
                   (c/to-date-time (member-license/ends license)))
      (member-license/ends license)
      end)))


(defn- payment-amount
  [license start end]
  (let [tz (member-license/time-zone license)]
    (if (= end (date/end-of-month start tz))
      (member-license/rate license)
      (*
       ;; daily rate
       (/ (member-license/rate license)
          ;; number of days in month
          (t/day (t/last-day-of-the-month (c/to-date-time start))))
       ;; number of days between `start` and `end`
       (inc (t/in-days
             (t/interval (c/to-date-time start)
                         (c/to-date-time end))))))))


(defn- rent-payment-params
  [license period]
  (let [tz    (member-license/time-zone license)
        start (date/beginning-of-day period tz)
        end   (payment-end-date license start)]
    {:start             start
     :end               end
     :amount            (payment-amount license start end)
     :member-license-id (td/id license)}))


(defn create-payment-events
  [deps event period]
  (let [actives (active-licenses (->db deps) period)]
    (->> (mapv
          (fn [[member-license-id amount]]
            (let [license (d/entity (->db deps) member-license-id)
                  tz      (member-license/time-zone license)
                  start   (date/beginning-of-day period tz)
                  account (member-license/account license)]
              (when (should-create-rent-payment? (->teller deps) account start)
                (taoensso.timbre/info "creating payment for:"
                                      (:account/email account)
                                      (rent-payment-params license period))
                (event/job :rent-payment/create {:params       (rent-payment-params license period)
                                                 :triggered-by event}))))
          actives)
         (remove nil?))))


(defmethod dispatch/job :rent-payments/create-all [deps event params]
  (assert (:period params) "The time period to create payments for must be supplied!")
  (create-payment-events deps event (:period params)))


;; =============================================================================
;; Alert Unpaid Payments
;; =============================================================================


(defn- payment-period [payment tz]
  (str (date/short (date/tz-uncorrected (tpayment/period-start payment) tz))
       "-"
       (date/short (date/tz-uncorrected (tpayment/period-end payment) tz))))


;; =====================================
;; Internal Slack notification


(defn- fmt-payment [db i payment]
  (let [account      (-> payment tpayment/customer tcustomer/account)
        tz           (member-license/time-zone (member-license/by-account db account))
        days-overdue (t/in-days (t/interval
                                 (date/tz-uncorrected-dt (c/to-date-time (tpayment/period-start payment)) tz)
                                 (t/now)))]
    (format "%s. %s's (_%s_) rent for `%s` is overdue by *%s days* (_due %s_), and late fees will be assessed."
            (inc i)
            (account/short-name account)
            (account/email account)
            (payment-period payment tz)
            days-overdue
            (-> payment tpayment/due (date/tz-uncorrected tz) (date/short true)))))


(defmethod dispatch/report :rent-payments/alert-unpaid
  [deps event {:keys [payment-ids as-of]}]
  (let [payments (->> (apply td/entities (->db deps) payment-ids)
                      (map (partial tpayment/by-entity (->teller deps))))]
    (slack/send
     (->slack deps)
     {:uuid    (event/uuid event)
      :channel slack/ops}
     (sm/msg
      (sm/warn
       (sm/title "The following rent payments are overdue:")
       (sm/pretext "_I've gone ahead and notified each member of his/her late payment; this is just FYI._")
       (sm/text (->> payments
                     (sort-by tpayment/due)
                     (map-indexed (partial fmt-payment (->db deps)))
                     (interpose "\n")
                     (apply str))))))))


;; =====================================
;; Member email


(defn- rent-overdue-body [db payment hostname]
  (let [account (-> payment tpayment/customer tcustomer/account)
        tz      (member-license/time-zone (member-license/by-account db account))]
    (mm/msg
     (mm/greet (account/first-name account))
     (mm/p
      (format "I hope all is well. I wanted to check in because your <b>rent for %s is now overdue and past the grace period</b> (the grace period ended on %s). Please <a href='%s/login'>log in to your account</a> to pay your balance at your earliest opportunity."
              (payment-period payment tz)
              (date/short (date/tz-uncorrected (tpayment/due payment) tz) true)
              hostname))
     (mm/p "While you're there, I'd highly encourage you to enroll in <b>Autopay</b> so you don't have to worry about missing due dates and having late fees assessed in the future.")
     (mm/p "If you're having trouble remitting payment, please let us know so we can figure out how best to accommodate you.")
     mail/accounting-sig)))


(defmethod dispatch/notify :rent-payments/alert-unpaid
  [deps event {:keys [payment-id]}]
  (let [payment (tpayment/by-id (->teller deps) payment-id)]
    ;; "placeholder" payments, e.g. for Mission AirBnB accounts won't have a
    ;; customer. This keeps our alerts from erroring out.
    (when-let [customer (tpayment/customer payment)]
      (mailer/send
       (->mailer deps)
       (-> customer tcustomer/account account/email)
       (mail/subject "Your Rent is Overdue")
       (rent-overdue-body (->db deps) payment (->public-hostname deps))
       {:uuid (event/uuid event)
        :from mail/from-accounting}))))


;; =====================================
;; Dispatch report/notify events


(defmethod dispatch/job :rent-payments/alert-unpaid
  [deps event {:keys [payment-ids as-of] :as params}]
  (let [payments (->> (apply td/entities (->db deps) payment-ids)
                      (map (partial tpayment/by-entity (->teller deps))))]
    (assert (every? tpayment/rent? payments)
            "All payments must be rent payments; not processing.")
    (conj
     ;; notify each member
     (map #(event/notify (event/key event) {:params       {:payment-id (tpayment/id %)}
                                            :triggered-by event})
          payments)
     (event/report (event/key event) {:params       params
                                      :triggered-by event}))))


;; =============================================================================
;; due date upcoming
;; =============================================================================


(defn- payment-due-soon-body [deps payment as-of]
  (let [tz  (->> payment
                 tpayment/property
                 tproperty/timezone
                 t/time-zone-for-id)
        due (date/tz-uncorrected (tpayment/due payment) tz)]
    (mm/msg
     (mm/greet (-> payment tpayment/customer tcustomer/account account/first-name))
     (mm/p
      (format "This is a friendly reminder to let you know that your rent payment of $%.2f <b>must be made by %s</b> to avoid late fees."
              (tpayment/amount payment) (date/short due true)))
     (mm/p
      (format "Please <a href='%s/login'>log in to your account</a> to pay your rent as soon as possible." (->public-hostname deps)))
     mail/accounting-sig)))


(defmethod dispatch/notify :payment/due [deps event {:keys [payment-id as-of]}]
  (let [payment (tpayment/by-entity (->teller deps) (d/entity (->db deps) payment-id))]
    (assert (tpayment/rent? payment)
            "Can only work with rent payments; not processing.")
    (assert (not (tpayment/paid? payment))
            "This payment has already been paid.")
    (mailer/send
     (->mailer deps)
     (-> payment tpayment/customer tcustomer/account account/email)
     "Starcity: Your Rent is Due Soon"
     (payment-due-soon-body deps payment as-of)
     {:uuid (event/uuid event)
      :from mail/from-accounting})))
