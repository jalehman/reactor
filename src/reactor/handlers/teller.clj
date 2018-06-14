(ns reactor.handlers.teller
  (:require [reactor.dispatch :as dispatch]
            [reactor.handlers.common :refer :all]
            [teller.subscription :as tsubscription]
            [toolbelt.date :as date]
            [blueprints.models.event :as event]
            [teller.payment :as tpayment]
            [teller.plan :as tplan]
            [clj-time.core :as t]
            [clj-time.coerce :as c]
            [teller.property :as tproperty]))


(defmethod dispatch/job :teller/daily
  [deps event params]
  (event/job ::bill-subscriptions {:params       params
                                   :triggered-by event}))


(defmethod dispatch/job ::bill-subscriptions
  [deps event {:keys [t] :as params}]
  (let [subs (tsubscription/query (->teller deps) {:canceled false})]
    (->> (filter
          #(let [current (tsubscription/current-billing-date % t)]
            (= (date/beginning-of-day t) (date/beginning-of-day current)))
          subs)
         (map
          #(event/job ::bill-subscription
                      {:params       {:subscription-id (tsubscription/id %)
                                      :t               t}
                       :triggered-by event})))))


(defmethod dispatch/job ::bill-subscription
  [deps event {:keys [t subscription-id]}]
  (let [subs  (tsubscription/by-id (->teller deps) subscription-id)
        plan  (tsubscription/plan subs)
        tz    (t/time-zone-for-id (tproperty/timezone (tsubscription/property subs)))
        start (date/beginning-of-day t tz)
        end   (-> (c/to-date-time t)
                  (t/plus (t/months 1))
                  (t/minus (t/days 1))
                  c/to-date
                  (date/end-of-day tz))]
    ;; TODO: Assert that there's no payment with the same start & end date
    (let [py (tpayment/create! (tsubscription/customer subs)
                               (tplan/amount plan)
                               (tplan/payment-type plan)
                               {:source   (tsubscription/source subs)
                                :property (tsubscription/property subs)
                                :period   [start end]})]
      (tsubscription/add-payment! subs py))
    nil))
