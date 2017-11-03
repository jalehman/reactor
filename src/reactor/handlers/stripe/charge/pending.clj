(ns reactor.handlers.stripe.charge.pending
  (:require [blueprints.models.account :as account]
            [blueprints.models.event :as event]
            [blueprints.models.payment :as payment]
            [datomic.api :as d]
            [reactor.dispatch :as dispatch]
            [reactor.handlers.common :refer :all]
            [reactor.handlers.stripe.common :as common]
            [reactor.services.slack :as slack]
            [reactor.services.slack.message :as sm]
            [ribbon.event :as re]
            [toolbelt.date :as date]
            [toolbelt.datomic :as td]))


(defmethod dispatch/report :stripe.event.charge/pending
  [deps event {:keys [payment-id]}]
  (let [payment (d/entity (->db deps) payment-id)
        account (payment/account payment)]
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


(defmethod dispatch/stripe :stripe.event.charge/pending [deps event _]
  (let [se        (common/fetch-event (->stripe deps) event)
        charge-id (re/subject-id se)]
    (when-let [payment (payment/by-charge-id (->db deps) charge-id)]
      (cond-> []
        (= (payment/payment-for2 (->db deps) payment) :payment.for/rent)
        (conj (event/report (event/key event) {:params       {:payment-id (td/id payment)}
                                               :triggered-by event}))

        true
        (conj (payment/add-source payment (get-in (re/subject se) [:source :id])))))))
