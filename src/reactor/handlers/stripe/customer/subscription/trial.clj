(ns reactor.handlers.stripe.customer.subscription.trial
  (:require [blueprints.models.account :as account]
            [blueprints.models.event :as event]
            [mailer.core :as mailer]
            [mailer.message :as mm]
            [reactor.dispatch :as dispatch]
            [reactor.handlers.common :refer :all]
            [reactor.handlers.stripe.common :as common]
            [reactor.utils.mail :as mail]
            [taoensso.timbre :as timbre]
            [teller.customer :as tcustomer]
            [teller.event :as tevent]
            [teller.subscription :as tsubscription]))

(defmulti process
  (fn [deps event sub]
    (tsubscription/payment-type sub)))


(defmethod process :default [_ event sub]
  (timbre/warn :stripe.event.customer.subscription/trial-will-end
               {:uuid         (event/uuid event)
                :subscription (tsubscription/id sub)}))


(defmethod process :payment.type/rent [deps event sub]
  (let [account (-> sub tsubscription/customer tcustomer/account)]
    (mailer/send
     (->mailer deps)
     (account/email account)
     (mail/subject "Autopay Beginning Soon")
     (mm/msg
      (mm/greet (account/first-name account))
      (mm/p "This is a friendly reminder that, since you configured <b>autopay</b>, your first payment will be taking place on the <b>1st of the upcoming month</b>.")
      (mm/p "For more details, log in to your Starcity account "
            [:a {:href (format "%s/profile" (->dashboard-hostname deps))} "here"]
            ".")
      mail/accounting-sig)
     {:uuid (event/uuid event)
      :from mail/from-accounting})))


(defmethod dispatch/stripe :stripe.event.customer.subscription/trial-will-end
  [deps event params]
  (let [se  (common/fetch-event (->teller deps) event)
        sub (tevent/handle-stripe-event (->teller deps) se)]
    (process deps event sub)))
