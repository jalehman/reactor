(ns reactor.handlers.security-deposit
  (:require [blueprints.models.account :as account]
            [blueprints.models.security-deposit :as deposit]
            [blueprints.models.event :as event]
            [blueprints.models.member-license :as member-license]
            [blueprints.models.payment :as payment]
            [clojure.spec :as s]
            [datomic.api :as d]
            [reactor.dispatch :as dispatch]
            [reactor.handlers.common :refer :all]
            [reactor.services.slack :as slack]
            [reactor.services.slack.message :as sm]
            [ribbon.charge :as rc]
            [taoensso.timbre :as timbre]
            [toolbelt.core :as tb]
            [toolbelt.predicates :as p]
            [toolbelt.async :refer [<!!?]]
            [mailer.core :as mailer]
            [mailer.message :as mm]
            [clojure.string :as string]))


;; =============================================================================
;; First Payment
;; =============================================================================


(defmethod dispatch/report :deposit/payment-made [deps event {:keys [account-id charge]}]
  (let [account (d/entity (->db deps) account-id)
        payment (payment/by-charge-id (->db deps) charge)]
    (slack/send
     (->slack deps)
     {:uuid    (event/uuid event)
      :channel slack/ops}
     (sm/msg
      (sm/success
       (sm/title "View Payment on Stripe"
                 (format "https://dashboard.stripe.com/payments/%s" charge))
       (sm/text (format "%s has made a security deposit payment!"
                        (account/full-name account)))
       (sm/fields
        (sm/field "Amount" (str "$" (payment/amount payment)) true)))))))


(defmethod dispatch/job :deposit/payment-made [deps event params]
  (event/report (event/key event) {:params       params
                                   :triggered-by event}))


;; =============================================================================
;; Remainder Payment
;; =============================================================================


(defmethod dispatch/report :deposit.remainder/payment-made
  [deps event {:keys [account-id charge]}]
  (let [account (d/entity (->db deps) account-id)
        payment (payment/by-charge-id (->db deps) charge)]
    (slack/send
     (->slack deps)
     {:uuid    (event/uuid event)
      :channel slack/ops}
     (sm/msg
      (sm/success
       (sm/title "View Payment on Stripe"
                 (format "https://dashboard.stripe.com/payments/%s" charge))
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
     "Starcity: Security Deposit Refund"
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
      (mm/sig))
     {:uuid (event/uuid event)})))


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
;;         :args (s/cat :deps map? :payment p/entity?)
;;         :ret string?)


(defn refund-payment!
  "Refund `amount` from `payment`."
  [deps payment amount]
  (<!!? (rc/refund! (->stripe deps) (payment/charge-id payment)
                    :amount (float amount))))

(s/fdef refund-payment!
        :args (s/cat :deps map? :payment p/entity? :amount number?))


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
        :args (s/cat :deposit p/entity?
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
                     :event p/entity?
                     :deposit p/entity?
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
