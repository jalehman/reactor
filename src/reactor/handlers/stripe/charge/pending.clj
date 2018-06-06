(ns reactor.handlers.stripe.charge.pending
  (:require [blueprints.models.account :as account]
            [blueprints.models.event :as event]
            [blueprints.models.member-license :as member-license]
            [reactor.dispatch :as dispatch]
            [reactor.handlers.common :refer :all]
            [reactor.handlers.stripe.common :as common]
            [reactor.services.slack :as slack]
            [reactor.services.slack.message :as sm]
            [teller.customer :as tcustomer]
            [teller.event :as tevent]
            [teller.payment :as tpayment]
            [toolbelt.date :as date]))

(defmethod dispatch/report :stripe.event.charge/pending
  [deps event {:keys [payment-id]}]
  (let [payment (tpayment/by-id (->teller deps) payment-id)
        account (tcustomer/account (tpayment/customer payment))
        tz (member-license/time-zone (member-license/by-account (->db deps) account))]
    (slack/send
     (->slack deps)
     {:uuid    (event/uuid event)
      :channel slack/ops}
     (sm/msg
      (sm/success
       (sm/title "View Payment on Stripe")
       (sm/text (format "%s has paid his/her rent via ACH" (account/full-name account)))
       (sm/fields
        (sm/field "Amount"
                  (str "$" (tpayment/amount payment))
                  true)
        (sm/field "Period Start"
                  (date/short-date (date/tz-uncorrected (tpayment/period-start payment) tz))
                  true)
        (sm/field "Period End"
                  (date/short-date (date/tz-uncorrected (tpayment/period-end payment) tz))
                  true)))))))


(defmethod dispatch/stripe :stripe.event.charge/pending [deps event _]
  (let [se      (common/fetch-event (->teller deps) event)
        payment (tevent/handle-stripe-event (->teller deps) se)]
    (when (tpayment/rent? payment)
      (event/report (event/key event) {:params       {:payment-id (tpayment/id payment)}
                                       :triggered-by event}))))
