(ns reactor.handlers.stripe.customer.subscription.trial
  (:require [blueprints.models.event :as event]
            [reactor.dispatch :as dispatch]
            [reactor.handlers.common :refer :all]
            [reactor.handlers.stripe.common :as common]
            [ribbon.event :as re]
            [taoensso.timbre :as timbre]
            [blueprints.models.member-license :as member-license]
            [mailer.core :as mailer]
            [blueprints.models.account :as account]
            [mailer.message :as mm]))


(defmulti process
  (fn [deps event stripe-event]
    (common/subscription-type (->db deps) (re/subject-id stripe-event))))


(defmethod process :default [_ event stripe-event]
  (timbre/warn :stripe.event.customer.subscription/trial-will-end
               {:uuid         (event/uuid event)
                :subscription (re/subject-id stripe-event)}))


(defmethod process :rent [deps event stripe-event]
  (let [license (member-license/by-subscription-id (->db deps) (re/subject-id stripe-event))
        account (member-license/account license)]
    (mailer/send
     (->mailer deps)
     (account/email account)
     "Starcity: Autopay Beginning Soon"
     (mm/msg
      (mm/greet (account/first-name account))
      (mm/p "This is a friendly reminder that, since you configured <b>autopay</b>, your first payment will be taking place on the <b>1st of the upcoming month</b>.")
      (mm/p "For more details, log in to your Starcity account "
            [:a {:href (format "%s/me/account/rent" (->public-hostname deps))} "here"]
            ".")
      (mm/sig))
     {:uuid (event/uuid event)})))


(defmethod dispatch/stripe :stripe.event.customer.subscription/trial-will-end
  [deps event params]
  (let [stripe-event (common/fetch-event (->stripe deps) event)]
    (process deps event stripe-event)))
