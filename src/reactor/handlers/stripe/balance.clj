(ns reactor.handlers.stripe.balance
  (:require [blueprints.models.event :as event]
            [blueprints.models.transaction :as transaction]
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
            [datomic.api :as d]
            [cheshire.core :as json]
            [toolbelt.core :as tb]))


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
       (map string/upper-case)
       (interpose " ")
       (apply str)))


(defn create-payout!
  [stripe desc {:keys [txn-id amount currency source]} & {:keys [email]}]
  (<!!? (payout/create! stripe amount
                        :description desc
                        :statement-descriptor desc
                        :currency currency
                        :source-type "bank_account"
                        :metadata (tb/assoc-when
                                   {:txn_id txn-id
                                    :source source}
                                   :email email))))


(defn create-payment-backed-payout [deps payment txn]
  (let [payout (create-payout! (->stripe deps)
                               (descriptor-for (->db deps) payment txn)
                               (account/email (payment/account payment))
                               txn)]
    [(transaction/create (:txn-id txn) (:source txn)
                         :payment payment
                         :payout-id (:id payout))]))


(defn create-unknown-payout [deps txn]
  (let [payout (create-payout! (->stripe deps) "UNKNOWN" txn)]
    [(transaction/create (:txn-id txn) (:source txn)
                         :payout-id (:id payout))]))


(defmethod dispatch/job ::create-payout [deps event {:keys [source] :as txn}]
  (with-connect-account (common/connect-account event)
    (if-let [payment (payment/by-charge-id (->db deps) source)]
      (create-payment-backed-payout deps payment txn)
      (create-unknown-payout deps txn))))


;; =============================================================================
;; Balance Available
;; =============================================================================


;; 1. Inspect `sevent` for an available balance with a positive available balance


(defn has-available-balance? [stripe-event]
  (let [available (get-in stripe-event [:data :object :available])
        total     (reduce #(+ %1 (:amount %2)) 0 available)]
    (> total 0)))


;; 2. When positive, fetch balance history (`ribbon.balance/list-all`) for last
;; three days (set `limit` to `100`).

(def days-back "Number of days to search back for." 3)
(def num-transactions "Number of transactions to include in query." 100)


(defn fetch-transaction-history
  [stripe-conn & [days-back]]
  (let [dt (t/minus (t/now) (t/days (or days-back reactor.handlers.stripe.balance/days-back)))]
    (:data (<!!? (balance/list-all stripe-conn
                                   :available-on {:gte (c/to-date dt)}
                                   :limit num-transactions)))))


;; 3. Filter all transactions by those with type #{charge, application_fee,
;; payment}


(defn only-inbound-transactions
  [db transactions]
  (filter
   (fn [{:keys [type status source] :as txn}]
     (if-let [tx (transaction/by-source-id db source)]
       (some? (transaction/payout-id tx))
       (and (#{"charge" "application_fee" "payment"} type)
            (= "available" status))))
   transactions))

;; 4. For each transaction, create a payout in the amount of the `:net`
;;      - The `:source` key is the charge/payment id


(defn payout-events
  [event transactions]
  (mapv
   (fn [{:keys [id net source currency type] :as txn}]
     (event/job ::create-payout (tb/assoc-when
                                 {:params       {:txn-id   id
                                                 :amount   net
                                                 :currency currency
                                                 :source   source
                                                 :type     type}
                                  :triggered-by event}
                                 :meta (event/metadata event))))
   transactions))


(defmethod dispatch/stripe :stripe.event.balance/available [deps event _]
  (with-connect-account (common/connect-account event)
    (let [sevent (common/event->stripe (->stripe deps) event)]
      (when (has-available-balance? sevent)
        (->> (fetch-transaction-history (->stripe deps))
             (only-inbound-transactions (->db deps))
             (payout-events event))))))


(comment

  (def secret-key "sk_test_mPUtCMOnGXJwD6RAWMPou8PH")

  (let [deps  {:stripe secret-key}
        conn  reactor.datomic/conn
        event {:event/id   "evt_1BlQJbJDow24Tc1aLHvm3994"
               :event/meta (pr-str {:managed-account "acct_191838JDow24Tc1a"})}]
    (with-connect-account (common/connect-account event)
      (let [sevent (common/event->stripe secret-key event)
            ev   (->> (fetch-transaction-history (->stripe deps) 10)
                      (only-inbound-transactions (d/db conn))
                      (payout-events event)
                      last)]
        ;; @(d/transact conn [ev])
        )))


  (d/transact reactor.datomic/conn [(event/stripe :stripe.event.balance/available {:id "evt_1BlQJbJDow24Tc1aLHvm3994"})])


  (fetch-transaction-history secret-key)


  (with-connect-account "acct_191838JDow24Tc1a"
    (<!!? (balance/retrieve secret-key)))

  )
