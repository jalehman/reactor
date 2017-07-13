(ns reactor.events
  (:require [blueprints.models
             [event :as event]
             [note :as note]]
            [clojure.spec :as s]
            [toolbelt.core :as tb]
            [clojure.string :as string]
            [toolbelt.datomic :as td]
            [toolbelt.predicates :as p]))


;; =============================================================================
;; Accounts
;; =============================================================================


(defn reset-password
  "Reset `account`'s password."
  [account]
  (event/job :account/reset-password {:params {:account-id (td/id account)}}))

(s/fdef reset-password
        :args (s/cat :account p/entity?)
        :ret map?)


(defn create-account
  "Create a new account."
  [email password first-name last-name & {:keys [middle-name]}]
  (event/job :account/create {:params (tb/assoc-when
                                       {:email      email
                                        :password   password
                                        :first-name first-name
                                        :last-name  last-name}
                                       :middle-name middle-name)}))

(s/fdef create-account
        :args (s/cat :email string?
                     :password string?
                     :first-name string?
                     :last-name string?
                     :opts (s/keys* :opt-un [::middle-name]))
        :ret map?)


(defn create-collaborator
  "Create a new collaborator."
  [email type message]
  (event/job :collaborator/create {:params {:email   email
                                            :type    type
                                            :message message}}))

(s/fdef create-collaborator
        :args (s/cat :email string?
                     :type #{"real-estate" "community-stakeholder" "vendor" "investor"}
                     :message string?)
        :ret map?)


(defn account-promoted
  "`account` has been promoted to membership."
  [account]
  (event/job :account/promoted {:params {:account-id (td/id account)}}))

(s/fdef account-promoted
        :args (s/cat :account p/entity?)
        :ret map?)


(defn account-approved
  "`account` has been approved for membership."
  [account]
  (event/job :account/approved {:params {:account-id (td/id account)}}))

(s/fdef account-approved
        :args (s/cat :account p/entity?)
        :ret map?)


;; =============================================================================
;; Deposit
;; =============================================================================


(defn deposit-payment-made
  [account charge-id]
  (event/job :deposit/payment-made {:params {:account-id (td/id account)
                                             :charge     charge-id}}))

(s/fdef deposit-payment-made
        :args (s/cat :account p/entity? :charge-id string?)
        :ret map?)


(defn remainder-deposit-payment-made
  [account charge-id]
  (event/job :deposit.remainder/payment-made {:params {:account-id (td/id account)
                                                       :charge     charge-id}}))

(s/fdef remainder-deposit-payment-made
        :args (s/cat :account p/entity? :charge-id string?)
        :ret map?)


;; =============================================================================
;; Newsletter
;; =============================================================================


(defn add-newsletter-subscriber
  "Add `email` address to our newsletter."
  [email]
  (event/job :newsletter/subscribe {:params {:email email}}))

(s/fdef add-newsletter-subscriber
        :args (s/cat :email string?)
        :ret map?)


;; =============================================================================
;; Notes
;; =============================================================================


(defn note-created
  "A `note` was created."
  [note]
  (event/job :note/created {:params {:uuid (note/uuid note)}}))

(s/fdef note-created
        :args (s/cat :note p/entity?)
        :ret map?)


(defn note-comment-created
  "A `comment` has been added to a `note`."
  [note comment]
  (event/job :note.comment/created {:params {:comment-uuid (note/uuid comment)
                                             :note-id      (td/id note)}}))

(s/fdef added-note-comment
        :args (s/cat :note p/entity? :comment p/entity?)
        :ret map?)


;; =============================================================================
;; Orders
;; =============================================================================


(defn place-order
  "Place an `order`."
  [order]
  (event/job :order/place {:params {:order-id (td/id order)}}))

(s/fdef place-order
        :args (s/cat :order p/entity?)
        :ret map?)


;; =============================================================================
;; Rent
;; =============================================================================


(defn create-monthly-rent-payments
  "Create rent payments for the current time `period` for members that are not
  on autopay."
  [period]
  (event/job :rent-payments/create-all {:params {:period period}}))

(s/fdef create-monthly-rent-payments
        :args (s/cat :period inst?)
        :ret map?)


(defn rent-payment-made
  "A member has paid his/her rent by ACH."
  [account rent-payment]
  (event/job :rent-payment.payment/ach {:params {:account-id (td/id account)
                                                 :payment-id (td/id rent-payment)}}))

(s/fdef rent-payment-made
        :args (s/cat :account p/entity? :payment p/entity?)
        :ret map?)


;; =============================================================================
;; Session
;; =============================================================================


(defn revoke-session
  "Revoke `account`'s session."
  [account]
  (event/job :session/revoke {:params {:account-id (td/id account)}}))

(s/fdef revoke-session
        :args (s/cat :account p/entity?)
        :ret map?)


;; =============================================================================
;; Stripe
;; =============================================================================


(defn- snake->kebab [s]
  (string/replace s #"_" "-"))


(defn- event-type->key [et]
  (let [parts (->> (concat '("stripe" "event") (string/split et #"\."))
                   (map snake->kebab))]
    (keyword
     (string/join "." (butlast parts))
     (last parts))))

(s/fdef event-type->key
        :args (s/cat :type string?)
        :ret keyword?)


(defn stripe-event
  [event-id event-type connect-id]
  (let [meta (when-some [x connect-id] {:managed-account x})]
    (event/stripe (event-type->key event-type)
                  (tb/assoc-when {:id event-id} :meta meta))))

(s/fdef stripe-event
        :args (s/cat :event-id string?
                     :event-type string?
                     :connect-id (s/or :string string? :nothing nil?))
        :ret map?)
