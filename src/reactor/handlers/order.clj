(ns reactor.handlers.order
  (:require [blueprints.models.account :as account]
            [blueprints.models.customer :as customer]
            [blueprints.models.event :as event]
            [blueprints.models.order :as order]
            [blueprints.models.payment :as payment]
            [blueprints.models.service :as service]
            [blueprints.models.source :as source]
            [clojure.core.async :refer [<!!]]
            [datomic.api :as d]
            [reactor.dispatch :as dispatch]
            [reactor.handlers.common :refer :all]
            [ribbon.charge :as rc]
            [ribbon.customer :as rcu]
            [ribbon.plan :as rp]
            [ribbon.subscription :as rs]
            [toolbelt.async :refer [<!!?]]
            [toolbelt.datomic :as td]
            [toolbelt.predicates :as p]
            [taoensso.timbre :as timbre]))

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
;; Place Order
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
    (if (p/throwable? existing)
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
