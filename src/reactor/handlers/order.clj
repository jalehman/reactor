(ns reactor.handlers.order
  (:require [blueprints.models.account :as account]
            [blueprints.models.customer :as customer]
            [blueprints.models.event :as event]
            [blueprints.models.member-license :as member-license]
            [blueprints.models.order :as order]
            [blueprints.models.payment :as payment]
            [blueprints.models.property :as property]
            [blueprints.models.service :as service]
            [blueprints.models.source :as source]
            [clojure.core.async :refer [<!!]]
            [datomic.api :as d]
            [mailer.core :as mailer]
            [mailer.message :as mm]
            [reactor.dispatch :as dispatch]
            [reactor.handlers.common :refer :all]
            [reactor.services.slack :as slack]
            [reactor.services.slack.message :as sm]
            [reactor.utils.mail :as mail]
            [ribbon.charge :as rc]
            [ribbon.customer :as rcu]
            [ribbon.plan :as rp]
            [ribbon.subscription :as rs]
            [taoensso.timbre :as timbre]
            [toolbelt.async :refer [<!!?]]
            [toolbelt.core :as tb]
            [toolbelt.date :as date]
            [toolbelt.datomic :as td]))

;; =============================================================================
;; Helpers
;; =============================================================================


(defn- credit-card [deps customer-id]
  (let [customer (rcu/fetch (->stripe deps) customer-id)
        card     (rcu/active-credit-card (<!!? customer))]
    (if (nil? card)
      (throw (ex-info "Cannot place order; customer has no credit card!"
                      {:customer customer-id}))
      card)))


(defn- stripe-desc [account order]
  (let [email (account/email account)
        quant (or (order/quantity order) 1)
        code  (-> order order/service service/code)]
    (if-some [v (get-in order [:order/variant :svc-variant/name])]
      (format "%s : x%s : %s (%s)" email quant code v)
      (format "%s : x%s : %s" email quant code))))


;; =============================================================================
;; Process Order
;; =============================================================================


(defmulti process-order
  (fn [deps event order account]
    (-> order order/service service/billed)))


(defmethod process-order :default [_ event order _]
  (let [account (order/account order)]
    (throw (ex-info "This order has an unknown billing method; cannot place!"
                    {:order   (td/id order)
                     :event   (td/id event)
                     :account (account/email account)}))))


;;; Billed Once


(defn- issue-charge! [deps account order price]
  (let [cus-id (customer/id (customer/by-account (->db deps) account))
        card   (rcu/token (credit-card deps cus-id))
        desc   (stripe-desc account order)
        price  (-> price (* 100) int)]
    (:id (<!!? (rc/create! (->stripe deps) price card
                           :customer-id cus-id
                           :description desc
                           :email (account/email account))))))


(defmethod process-order :service.billed/once
  [deps event order _]
  (let [account (order/account order)
        price   (* (order/computed-price order) (or (order/quantity order) 1))
        ch-id   (issue-charge! deps account order price)
        py      (payment/create price account
                                :for :payment.for/order
                                :charge-id ch-id)]
    [(order/add-payment order py)
     (order/is-charged order)
     [:db/add (:db/id order) :order/billed-on (java.util.Date.)]
     py]))


;;; Billed Monthly


(defmethod dispatch/job ::create-subscription
  [deps event {:keys [order-id plan-id account-id]}]
  (let [order   (d/entity (->db deps) order-id)
        account (order/account order)
        cus-id  (customer/id (customer/by-account (->db deps) account))
        sub     (<!!? (rs/create! (->stripe deps) cus-id plan-id
                                  :quantity (int (or (order/quantity order) 1))))]
    [{:db/id          order-id
      :stripe/plan-id plan-id
      :stripe/subs-id (:id sub)}
     [:db/add (:db/id order) :order/billed-on (java.util.Date.)]
     (order/is-charged order)
     (source/create account-id)]))


;; Uses <service-code>-<price-in-cents> as a template for constructing unique
;; plan ids. This way we can introduce subscriptions to the same "service" but
;; with different pricing, as it may be quote-based.

(defn- fetch-or-create-plan [deps account order]
  (let [price     (int (* 100 (order/computed-price order)))
        plan-name (-> order order/service service/code)
        plan-id   (str plan-name "-" price)
        existing  (<!! (rp/fetch (->stripe deps) plan-id))]
    (if (tb/throwable? existing)
      (<!!? (rp/create! (->stripe deps) plan-id plan-name price :month))
      existing)))


(defmethod dispatch/job ::create-plan [deps event {:keys [order-id account-id]}]
  (let [order   (d/entity (->db deps) order-id)
        account (order/account order)
        plan    (fetch-or-create-plan deps account order)]
    (event/job ::create-subscription {:params       {:order-id   order-id
                                                     :plan-id    (:id plan)
                                                     :account-id account-id}
                                      :triggered-by event})))


(defmethod process-order :service.billed/monthly [deps event order initiator]
  (let [account  (order/account order)
        cus-id   (customer/id (customer/by-account (->db deps) account))
        customer (<!!? (rcu/fetch (->stripe deps) cus-id))]
    (assert (#{"card"} (rcu/default-source-type customer))
            "Customer's default source must be a credit card before a subscription can be created.")
    (event/job ::create-plan {:params       {:order-id   (td/id order)
                                             :account-id (td/id initiator)}
                              :triggered-by event})))


;;; Entrypoint


(defmethod dispatch/job :order/process [deps event {:keys [order-id account-id]}]
  (let [order   (d/entity (->db deps) order-id)
        account (d/entity (->db deps) account-id)]
    (try
      (assert (order/processing? order) "Order must be in `processing` status!")
      (assert (some? (order/computed-price order)) "Order cannot be processed without a price!")
      (conj (process-order deps event order account) (source/create account-id))
      (catch Throwable t
        (timbre/error t :order/process {:order-id order-id :account-id account-id})
        (throw (ex-info "Error encountered while attempting to process order!"
                        {:order-id   order-id
                         :account-id account-id
                         :tx         [(order/is-failed order)]}))))))


;; =============================================================================
;; Other Lifecycle
;; =============================================================================


(defn- order-name [order]
  (-> order order/service service/name))


(defn- time-zone [db account]
  (member-license/time-zone (member-license/by-account db account)))


;; created ==============================


(defn- rand-planet-express []
  (let [phrases ["Good news, everyone!" "Neat." "Sweet bongo of the congo!"
                 "Arrrooooooo!" "Hooray!" "Huzzah!" "Bam!" "Yup."]]
    (->> phrases count rand-int (get phrases))))


(defn- order-url [hostname order]
  (format "%s/services/orders/%s" hostname (:db/id order)))


(def ^:private property-channel
  {"52gilbert"   "#52-gilbert"
   "2072mission" "#2072-mission"
   "6nottingham" "#6-nottingham"})


(defn- notification-channel [db account]
  (let [code (property/code (account/current-property db account))]
    (get property-channel code slack/crm)))


(defmethod dispatch/notify :order/created
  [deps event {:keys [order-uuid account-id]}]
  (let [order   (order/by-uuid (->db deps) order-uuid)
        creator (d/entity (->db deps) account-id)
        ;; this is who's getting the order
        member  (order/account order)
        tz      (time-zone (->db deps) member)]

    ;; email notification -> to member
    (mailer/send
     (->mailer deps)
     (account/email member)
     (mail/subject (format "Your order for %s has been created" (order-name order)))
     (mm/msg
      (mm/greet (account/first-name member))
      (mm/p
       (format "Your order for %s has been created%s and is now <i>pending</i>. If you change your mind, you can still cancel it before it is <i>placed</i>."
               (order-name order)
               (if (not= creator member) (str " by " (account/short-name creator)) "")))
      (mm/p "Your community representative will reach out to you shortly to confirm your order. Once it is confirmed, your order will be <i>placed</i>, at which point it can no longer be canceled.")
      (mm/sig))
     {:uuid (event/uuid event)})))


(defmethod dispatch/report :order/created
  [deps event {:keys [order-uuid account-id]}]
  (let [order   (order/by-uuid (->db deps) order-uuid)
        creator (d/entity (->db deps) account-id)
        ;; this is who's getting the order
        member  (order/account order)
        tz      (time-zone (->db deps) member)]

    ;; slack notification -> to community team (when a member makes a request)
    (when (= (:db/id member) (:db/id creator))
      (slack/send
       (->slack deps)
       {:uuid    (event/uuid event)
        :channel (notification-channel (->db deps) member)}
       (sm/msg
        (sm/info
         (sm/title "New Premium Service Order"
                   (order-url (->dashboard-hostname deps) order))
         (sm/text (format "%s %s has just placed a Premium Service Order!" (rand-planet-express) (account/short-name member)))
         (sm/fields
          (sm/field "Member" (account/short-name member))
          (sm/field "Service" (order-name order)))))))))


(defmethod dispatch/job :order/created
  [deps event {:keys [account-id notify] :as params}]
  (when notify
    [(event/notify (event/key event) {:params       params
                                      :triggered-by event})
     (event/report (event/key event) {:params       params
                                      :triggered-by event})
     (source/create account-id)]))


(comment
  ;; make a notification event pending - evaluate the buffer, then this line
  ;; this one came from the admin side
  @(d/transact conn [[:db/add 285873023223562 :event/status :event.status/pending]])



  ;; this one came from the member side
  @(d/transact conn [[:db/add 285873023223641 :event/status :event.status/pending]])

  ;; then go check debug in slack

  )


;; placed ==============================


(defmethod dispatch/notify :order/placed
  [deps event {:keys [order-id account-id]}]
  (let [[order placed-by] (td/entities (->db deps) order-id account-id)
        orderer           (order/account order)
        tz                (time-zone (->db deps) orderer)]
    (mailer/send
     (->mailer deps)
     (account/email orderer)
     (mail/subject (format "Your order for %s has been placed" (order-name order)))
     (mm/msg
      (mm/greet (account/first-name orderer))
      (mm/p
       (format "Your order for %s has been placed by %s, which means that it can no longer be canceled. If this comes as a surprise, please reach out to %s at %s directly."
               (order-name order)
               (account/short-name placed-by)
               (account/first-name placed-by)
               (account/email placed-by)))
      (when-let [date (order/projected-fulfillment order)]
        (mm/p (format "Your order is expected to be fulfilled at <b>%s</b>."
                      (-> date
                          (date/from-tz-date tz)
                          (date/short-date-time)))))
      (mm/sig))
     {:uuid (event/uuid event)})))


(defmethod dispatch/job :order/placed
  [deps event {:keys [account-id notify] :as params}]
  (when notify
    [(event/notify (event/key event) {:params       params
                                      :triggered-by event})
     (source/create account-id)]))


;; =====================================
;; Fulfulled


(defmethod dispatch/notify :order/fulfilled
  [deps event {:keys [order-id account-id]}]
  (let [[order placed-by] (td/entities (->db deps) order-id account-id)
        orderer           (order/account order)
        tz                (time-zone (->db deps) orderer)]
    (mailer/send
     (->mailer deps)
     (account/email orderer)
     (mail/subject (format "Your order for %s has been fulfilled" (order-name order)))
     (mm/msg
      (mm/greet (account/first-name orderer))
      (mm/p
       (format "Your order for %s has been fulfilled by %s on <b>%s</b>. If this comes as a surprise, please reach out to %s at %s directly."
               (order-name order)
               (account/short-name placed-by)
               (-> (order/fulfilled-on order)
                   (date/from-tz-date tz)
                   date/short-date-time)
               (account/first-name placed-by)
               (account/email placed-by)))
      (mm/sig))
     {:uuid (event/uuid event)})))


(defmethod dispatch/job :order/fulfilled
  [deps event {:keys [account-id notify] :as params}]
  (when notify
    [(event/notify (event/key event) {:params       params
                                      :triggered-by event})
     (source/create account-id)]))


;; =====================================
;; Canceled


(defmethod dispatch/notify :order/canceled
  [deps event {:keys [order-id account-id]}]
  (let [[order placed-by] (td/entities (->db deps) order-id account-id)
        orderer           (order/account order)]
    (mailer/send
     (->mailer deps)
     (account/email orderer)
     (mail/subject (format "Your order for %s has been canceled" (order-name order)))
     (mm/msg
      (mm/greet (account/first-name orderer))
      (mm/p
       (format "Your order for %s has been canceled by %s. If this comes as a surprise, please reach out to %s at %s directly."
               (order-name order)
               (account/short-name placed-by)
               (account/first-name placed-by)
               (account/email placed-by)))
      (mm/sig))
     {:uuid (event/uuid event)})))


(defmethod dispatch/report :order/canceled
  [deps event {:keys [order-id account-id]}]
  (let [[order placed-by] (td/entities (->db deps) order-id account-id)
        orderer           (order/account order)]

    ;; slack notification -> to community team (when a member makes a request)
    (slack/send
     (->slack deps)
     {:uuid    (event/uuid event)
      :channel (notification-channel (->db deps) orderer)}
     (sm/msg
      (sm/info
       (sm/title "Helping Hands Order Canceled"
                 (order-url (->dashboard-hostname deps) order))
       (sm/text (format "%s has just canceled a Helping Hands Order. :sadparrot:" (account/short-name orderer)))
       (sm/fields
        (sm/field "Member" (account/short-name orderer))
        (sm/field "Service" (order-name order))))))))


(defmethod dispatch/job :order/canceled
  [deps event {:keys [account-id notify] :as params}]
  (when notify
    [(event/notify (event/key event) {:params       params
                                      :triggered-by event})
     (event/report (event/key event) {:params       params
                                      :triggered-by event})
     (source/create account-id)]))


(comment

  ;; (def conn reactor.datomic/conn)

  (def conn odin.datomic/conn)

  @(d/transact conn [(order/create [:account/email "member@test.com"] (service/by-code (d/db conn) "box-fan"))])

  (let [order (order/by-account (d/db conn) [:account/email "member@test.com"] (service/by-code (d/db conn) "box-fan"))]
    @(d/transact conn [#_[:db/add (:db/id order) :order/fulfilled-on (java.util.Date.)]
                       #_[:db/add (:db/id order) :order/projected-fulfillment (java.util.Date.)]
                       #_(blueprints.models.events/order-fulfilled [:account/email "admin@test.com"] order true)
                       (blueprints.models.events/order-placed [:account/email "admin@test.com"] order true)
                       #_(blueprints.models.events/order-canceled [:account/email "admin@test.com"] order true)]))

  )
