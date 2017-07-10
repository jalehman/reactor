(ns reactor.handlers.security-deposit
  (:require [blueprints.models.account :as account]
            [blueprints.models.charge :as charge]
            [blueprints.models.event :as event]
            [datomic.api :as d]
            [reactor.dispatch :as dispatch]
            [reactor.handlers.common :refer :all]
            [reactor.services.slack :as slack]
            [reactor.services.slack.message :as sm]))


;; =============================================================================
;; First Payment
;; =============================================================================


(defmethod dispatch/report :deposit/payment-made [deps event {:keys [account-id charge]}]
  (let [account (d/entity (->db deps) account-id)
        charge  (charge/by-id (->db deps) charge)]
    (slack/send
     (->slack deps)
     {:uuid    (event/uuid event)
      :channel slack/ops}
     (sm/msg
      (sm/success
       (sm/title "View Payment on Stripe"
                 (format "https://dashboard.stripe.com/payments/%s" (charge/id charge)))
       (sm/text (format "%s has made a security deposit payment!"
                        (account/full-name account)))
       (sm/fields
        (sm/field "Amount" (str "$" (charge/amount charge)) true)))))))


(defmethod dispatch/job :deposit/payment-made [deps event params]
  (event/report (event/key event) {:params       params
                                   :triggered-by event}))


;; =============================================================================
;; First Payment
;; =============================================================================


(defmethod dispatch/report :deposit.remainder/payment-made
  [deps event {:keys [account-id charge]}]
  (let [account (d/entity (->db deps) account-id)
        charge  (charge/by-id (->db deps) charge)]
    (slack/send
     (->slack deps)
     {:uuid    (event/uuid event)
      :channel slack/ops}
     (sm/msg
      (sm/success
       (sm/title "View Payment on Stripe"
                 (format "https://dashboard.stripe.com/payments/%s" (charge/id charge)))
       (sm/text (format "%s has paid the remainder of his/her security deposit"
                        (account/full-name account)))
       (sm/fields
        (sm/field "Method" "ACH" true)
        (sm/field "Amount" (format "$%.2f" (charge/amount charge)) true)))))))


(defmethod dispatch/job :deposit.remainder/payment-made [deps event params]
  (event/report (event/key event) {:params       params
                                   :triggered-by event}))
