(ns reactor.handlers.account
  (:require [blueprints.models.account :as account]
            [blueprints.models.approval :as approval]
            [blueprints.models.event :as event]
            [blueprints.models.events :as events]
            [blueprints.models.license :as license]
            [blueprints.models.member-license :as member-license]
            [blueprints.models.order :as order]
            [blueprints.models.property :as property]
            [blueprints.models.unit :as unit]
            [clojure.string :as string]
            [customs.auth :as auth]
            [datomic.api :as d]
            [mailer.core :as mailer]
            [mailer.message :as mm]
            [mailer.senders :as senders]
            [reactor.dispatch :as dispatch]
            [reactor.handlers.common :refer :all]
            [reactor.services.slack :as slack]
            [reactor.services.slack.message :as sm]
            [ring.util.codec :refer [url-encode]]
            [toolbelt.core :as tb]
            [toolbelt.date :as date]
            [toolbelt.datomic :as td]))


;; =============================================================================
;; Helpers
;; =============================================================================


(defn- unit-link [hostname unit]
  (let [property (unit/property unit)
        url      (format "%s/admin/properties/%s/units/%s"
                         hostname (:db/id property) (:db/id unit))]
    (sm/link url (:unit/name unit))))


(defn- property-link [hostname property]
  (let [url (format "%s/admin/properties/%s" hostname (:db/id property))]
    (sm/link url (property/name property))))


;; =============================================================================
;; Account Creation
;; =============================================================================


(defmethod dispatch/notify :account/create [deps event {:keys [email]}]
  (let [account (account/by-email (->db deps) email)]
    (mailer/send
     (->mailer deps)
     (account/email account)
     "Starcity: Activate Your Account"
     (mm/msg
      (mm/greet (account/first-name account))
      (mm/p "Thanks for signing up!")
      (mm/p (format "<a href='%s/signup/activate?email=%s&hash=%s'>Click here to activate your account</a> and apply for a home."
                    (->public-hostname deps)
                    (url-encode (account/email account))
                    (account/activation-hash account)))
      (mm/sig))
     {:uuid (event/uuid event)})))


(defn- create-account
  [{:keys [email first-name middle-name last-name password]}]
  (assert (some? email) "`email` is required.")
  (assert (some? first-name) "`first-name` is required.")
  (assert (some? last-name) "`last-name` is required.")
  (assert (some? password) "`password` is required.")
  (tb/assoc-when
   {:db/id                   (d/tempid :db.part/starcity)
    :account/email           (string/trim email)
    :account/first-name      (-> first-name string/trim string/capitalize)
    :account/last-name       (-> last-name string/trim string/capitalize)
    :account/password        password
    :account/activation-hash (auth/make-activation-hash email)
    :account/activated       false
    :account/role            :account.role/applicant}
   :account/middle-name (when-some [x middle-name]
                          (-> x string/trim string/capitalize))))


(defmethod dispatch/job :account/create [_ event params]
  [(create-account params)
   (event/notify :account/create
                 {:params       (select-keys params [:email :first-name :last-name])
                  :triggered-by event})])


;; =============================================================================
;; Promotion
;; =============================================================================


;; =============================================================================
;; Services Ordered


(defn- fmt-order [index order]
  (let [{:keys [name desc price quantity billed]} (order/clientize order)
        price (if-some [p price] (str "$" price "/ea") "Quote")]
    (cond
      (some? quantity)
      (format "%d. [ `%s` | _%s_ | *x%s* ] _%s_ (billed %s)"
              (inc index)
              (string/upper-case name)
              price
              (int quantity)
              desc
              (clojure.core/name billed))

      :otherwise
      (format "%d. [ `%s` | _%s_ ] _%s_ (billed %s)"
              (inc index)
              (string/upper-case name)
              price
              desc
              (clojure.core/name billed)))))


(defmethod dispatch/report :account.promoted/order-summary [deps event params]
  (let [account (d/entity (->db deps) (:account-id params))
        orders  (order/orders (->db deps) account)]
    (when-not (empty? orders)
      (slack/send
       (->slack deps)
       {:uuid    (:event/uuid event)
        :channel slack/crm}
       (sm/msg
        (sm/info
         (sm/title (format "%s ordered premium services:" (account/full-name account))
                   (account-link (->public-hostname deps) account))
         (sm/text (->> (map-indexed fmt-order orders) (interpose "\n") (apply str)))
         (sm/fields
          (sm/field "Account" (account-link (->public-hostname deps) account)))))))))


;; =============================================================================
;; Report


(defmethod dispatch/report :account/promoted [deps event params]
  (let [account (d/entity (->db deps) (:account-id params))
        license (member-license/active (->db deps) account)]
    (slack/send
     (->slack deps)
     {:uuid    (event/uuid event)
      :channel slack/crm}
     (sm/msg
      (sm/info
       (sm/text (format "*%s* is now a member!" (account/full-name account)))
       (sm/fields
        (sm/field "Account" (account-link (->public-hostname deps) account) true)
        (sm/field "Unit" (unit-link (->public-hostname deps) (member-license/unit license)) true)))))))


;; =============================================================================
;; Notify


(defn- ^:private promotion-email-subject [property]
  (format "Starcity: Welcome to %s!" (property/name property)))


(defn- promotion-email-body [hostname account]
  (mm/msg
   (mm/greet (account/first-name account))
   (mm/p "Thank you for signing your membership agreement and submitting
      payment for your security deposit. <b>Now you're officially a Starcity
      member!</b> We're looking forward to having you join the community and
      can't wait to get to know you better.")
   (mm/p (format "The next step to getting settled in is to log into your <a
      href='%s/me'>member dashboard</a>. This is where you'll be able to pay
      your rent payments and the remainder of your security deposit (if
      applicable). You can choose to <a href='%s/me/account/rent'>enable
      autopay</a> or make individual rent payments going forward." hostname hostname))
   (mm/p "Please let us know if you have any questions about the move-in
      process or need assistance navigating the dashboard.")
   (mm/sig "Meg" "Head of Community")))


(defn- send-promotion-email [deps account event]
  (let [property (-> (member-license/active (->db deps) account) member-license/property)]
    (mailer/send (->mailer deps) (account/email account)
                 (promotion-email-subject property)
                 (promotion-email-body (->public-hostname deps) account)
                 {:from senders/meg
                  :uuid (event/uuid event)})))


(defmethod dispatch/notify :account/promoted [deps event params]
  (let [account (d/entity (->db deps) (:account-id params))]
    (send-promotion-email deps account event)))


(defmethod dispatch/job :account/promoted [deps event params]
  (let [account (d/entity (->db deps) (:account-id params))
        license (member-license/active (->db deps) account)]
    (if-not (some? license)
      (throw (ex-info "Member has no active license!" {:account (account/email account)}))
      [(event/notify :account/promoted
                     {:params params :triggered-by event})

       (event/report :account/promoted
                     {:params params :triggered-by event})

       (event/report :account.promoted/order-summary
                     {:params params :triggered-by event})])))


;; =============================================================================
;; Approve
;; =============================================================================


(defmethod dispatch/report :account/approved [deps event {account-id :account-id}]
  (let [approvee (d/entity (->db deps) account-id)
        approval (approval/by-account approvee)
        approver (approval/approver approval)
        unit     (approval/unit approval)
        license  (approval/license approval)
        property (unit/property unit)]
    (slack/send
     (->slack deps)
     {:uuid    (event/uuid event)
      :channel slack/crm}
     (sm/msg
      (sm/info
       (sm/text
        (format "*%s* has approved *%s* for membership! _Don't forget to send an email to let %s know that they've been approved._"
                (account/full-name approver)
                (account/full-name approvee)
                (account/first-name approvee)))
       (sm/fields
        (sm/field "Email" (account/email approvee) true)
        (sm/field "Property" (property-link (->public-hostname deps) property) true)
        (sm/field "Unit" (unit-link (->public-hostname deps) unit) true)
        (sm/field "Move-in" (date/short-date (approval/move-in approval)) true)
        (sm/field "Term" (format "%s months" (license/term license)) true)))))))


(defmethod dispatch/job :account/approved [deps event {account-id :account-id :as params}]
  (let [account (d/entity (->db deps) account-id)]
    [(event/report (event/key event) {:params params})
     (assoc (events/revoke-session account-id) :event/triggered-by (td/id event))]))


;; =============================================================================
;; Reset Password
;; =============================================================================


(defmethod dispatch/notify :account/reset-password
  [deps event {:keys [account-id new-password]}]
  (let [account (d/entity (->db deps) account-id)
        link    (format "%s/login?email=%s&next=/account"
                        (->public-hostname deps) (url-encode (account/email account)))]
    (mailer/send
     (->mailer deps)
     (account/email account)
     "Starcity: Password Reset"
     (mm/msg
      (mm/greet (account/first-name account))
      (mm/p "As requested, we have reset your password. Your temporary password is:")
      (mm/p (str "<b>" new-password "</b>"))
      (mm/p
       (format "After logging in <a href='%s'>here</a>, please change your password to something more memorable by clicking on <b>My Account</b> in the upper right-hand corner of the page." link))
      (mm/p "If this was not you, please contact us at <a href='mailto:team@starcity.com'>team@starcity.com</a>.")
      (mm/sig))
     {:uuid (event/uuid event)})))


(defmethod dispatch/job :account/reset-password [deps event {account-id :account-id}]
  (let [account                (d/entity (->db deps) account-id)
        [new-password tx-data] (auth/reset-password account)]
    [tx-data
     (event/notify (event/key event) {:params       {:new-password new-password
                                                     :account-id   account-id}
                                      :triggered-by event})]))
