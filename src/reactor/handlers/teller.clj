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
            [teller.property :as tproperty]
            [datomic.api :as d]))


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


(defn- billing-period
  [as-of tz]
  [(date/beginning-of-day as-of tz)
   (-> (c/to-date-time as-of)
       (t/plus (t/months 1))
       (t/minus (t/days 1))
       c/to-date
       (date/end-of-day tz))])


(defn- bill-subscription!
  [subs start end]
  (let [plan (tsubscription/plan subs)
        py   (tpayment/create! (tsubscription/customer subs)
                               (tplan/amount plan)
                               (tplan/payment-type plan)
                               {:source   (tsubscription/source subs)
                                :property (tsubscription/property subs)
                                :period   [start end]})]
    (tsubscription/add-payment! subs py)))


(defn- already-billed?
  [subs start end]
  (let [payments (tsubscription/payments subs)
        start    (c/to-date-time start)
        end      (c/to-date-time end)]
    (->> (filter
          (fn [payment]
            (let [ps (c/to-date-time (tpayment/period-start payment))
                  pe (c/to-date-time (tpayment/period-end payment))]
              (and (t/within? (t/minus ps (t/days 1)) (t/plus ps (t/days 1)) start)
                   (t/within? (t/minus pe (t/days 1)) (t/plus pe (t/days 1)) end))))
          payments)
         (seq))))


(defmethod dispatch/job ::bill-subscription
  [deps event {:keys [t subscription-id]}]
  (let [subs        (tsubscription/by-id (->teller deps) subscription-id)
        plan        (tsubscription/plan subs)
        tz          (t/time-zone-for-id (tproperty/timezone (tsubscription/property subs)))
        [start end] (billing-period t tz)]
    (when (already-billed? subs start end)
      (throw (ex-info "This subscription has already been billed in this payment period!"
                      {:subscription-id subscription-id
                       :period          [start end]})))
    (bill-subscription! subs start end)
    nil))


(comment

  (def conn odin.datomic/conn)

  @(d/transact conn [(event/job :teller/daily {:params {:t #inst "2018-06-14"}})])

  )
