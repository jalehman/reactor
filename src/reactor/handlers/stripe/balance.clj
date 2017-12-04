(ns reactor.handlers.stripe.balance
  (:require [blueprints.models.event :as event]
            [reactor.dispatch :as dispatch]
            [reactor.handlers.common :refer :all]
            [reactor.handlers.stripe.common :as common]
            [ribbon.core :refer [with-connect-account]]
            [ribbon.balance :as balance]
            [ribbon.event :as re]
            [ribbon.payout :as payout]
            [taoensso.timbre :as timbre]
            [toolbelt.async :refer [<!!?]]
            [clj-time.core :as t]
            [clj-time.coerce :as c]
            [blueprints.models.payment :as payment]
            [blueprints.models.account :as account]
            [blueprints.models.property :as property]
            [clojure.string :as string]
            [datomic.api :as d]))


;; =============================================================================
;; Create Payout
;; =============================================================================


(defn prune [s n]
  (apply str (take n (seq s))))


(defn- txn-desc [db payment {type :type}]
  (let [pf (payment/payment-for2 db payment)]
    (cond
      (= pf :payment.for/deposit)
      "SDEP"

      (and (= pf :payment.for/rent) (= type "application_fee"))
      "PMRNT"

      (and (= pf :payment.for/order) (= type "application_fee"))
      "PMORD"

      (= pf :payment.for/rent)
      "RNT"

      (= pf :payment.for/order)
      "ORD"

      :otherwise
      "NA")))


(defn- property-desc [_ payment _]
  (if-let [property (payment/property payment)]
    (-> (property/code property) (prune 7))
    "NA"))


(defn- person-desc [_ payment _]
  (if-let [account (payment/account payment)]
    (-> (str (first (account/first-name account))
             (account/last-name account))
        (prune 8))
    "NA"))


(defn descriptor-for
  [db payment transaction]
  (->> ((juxt txn-desc property-desc person-desc) db payment transaction)
       (string/upper-case)
       (interpose " ")
       (apply str)))


(defmethod dispatch/job ::create-payout
  [deps event {:keys [txn-id amount currency source] :as txn}]
  (with-connect-account (common/connect-account event)
    (let [payment (payment/by-charge-id (->db deps) source)
          desc    (descriptor-for (->db deps) payment txn)]
      (payout/create! amount
                      :description desc
                      :statement-descriptor desc
                      :currency currency
                      :metadata {:txn_id  txn-id
                                 :source  source
                                 :account (account/email (payment/account payment))}))))


;; =============================================================================
;; Balance Available
;; =============================================================================


;; 1. Inspect `sevent` for an available balance with a positive available balance


(defn has-available-balance? [stripe-event]
  (let [available (get-in stripe-event [:object :available])
        total     (reduce #(+ %1 (:amount %2)) 0 available)]
    (> total 0)))


;; 2. When positive, fetch balance history (`ribbon.balance/list-all`) for last
;; three days (set `limit` to `100`).

(def days-back "Number of days to search back for." 3)
(def num-transactions "Number of transactions to include in query." 100)


(defn fetch-transaction-history
  [stripe-conn]
  (let [dt (t/minus (t/now) (t/days days-back))]
    (:data (<!!? (balance/list-all stripe-conn
                                   :available-on {:gte (c/to-date dt)}
                                   :limit num-transactions)))))

;; 3. Filter all transactions by those with type #{charge, application_fee,
;; payment}


(defn only-inbound-transactions
  [transactions]
  (filter
   (fn [{:keys [type status] :as txn}]
     (and (#{"charge" "application_fee" "payment"} type)
          (= "available" status)))
   transactions))

;; 4. For each transaction, create a payout in the amount of the `:net`
;;      - The `:source` key is the charge/payment id


(defn payout-events
  [event transactions]
  (mapv
   (fn [{:keys [id net source currency] :as txn}]
     (event/job ::create-payout {:params       {:txn-id   id
                                                :amount   net
                                                :currency currency
                                                :source   source
                                                :type     type}
                                 :meta         (event/metadata event)
                                 :triggered-by event}))
   transactions))


(defmethod dispatch/stripe :stripe.event.balance/available [deps event _]
  (with-connect-account (common/connect-account event)
    (let [sevent (common/event->stripe (->stripe deps) event)]
      (when (has-available-balance? sevent)
        (->> (fetch-transaction-history (->stripe deps))
             (only-inbound-transactions)
             (payout-events event))))))

;; TODO:
;; Test this when there is some actual available balance


(comment

  (def secret-key "sk_test_mPUtCMOnGXJwD6RAWMPou8PH")

  (common/event->stripe secret-key {:event/id "evt_1BOvT5IvRccmW9nOYvyjkcvp"})

  (d/transact reactor.datomic/conn [(event/stripe :stripe.event.balance/available {:id "evt_1BOvT5IvRccmW9nOYvyjkcvp"})])


  (d/entity (d/db reactor.datomic/conn) 285873023223185)

  (fetch-transaction-history secret-key)

  )
