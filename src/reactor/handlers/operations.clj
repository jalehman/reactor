(ns reactor.handlers.operations
  (:require [blueprints.models.account :as account]
            [blueprints.models.event :as event]
            [blueprints.models.events :as events]
            [blueprints.models.license :as license]
            [blueprints.models.license-transition :as transition]
            [blueprints.models.member-license :as member-license]
            [blueprints.models.order :as order]
            [blueprints.models.property :as property]
            [blueprints.models.unit :as unit]
            [clj-time.coerce :as c]
            [clj-time.core :as t]
            [clostache.parser :as stache]
            [datomic.api :as d]
            [mailer.core :as mailer]
            [mailer.message :as mm]
            [markdown.core :as md]
            [reactor.config :as config :refer [config]]
            [reactor.dispatch :as dispatch]
            [reactor.handlers.common :refer :all]
            [reactor.services.slack :as slack]
            [reactor.services.slack.message :as sm]
            [reactor.utils.mail :as mail]
            [reactor.utils.tipe :as tipe]
            [teller.customer :as tcustomer]
            [teller.payment :as tpayment]
            [teller.plan :as tplan]
            [teller.property :as tproperty]
            [teller.source :as tsource]
            [teller.subscription :as tsubscription]
            [toolbelt.core :as tb]
            [toolbelt.date :as date]
            [toolbelt.datomic :as td]))

;; ==============================================================================
;; helpers ======================================================================
;; ==============================================================================


(defn- prorated-amount
  [rate starts]
  (let [starts         (c/to-date-time starts)
        days-in-month  (t/day (t/last-day-of-the-month starts))
        days-remaining (inc (- days-in-month (t/day starts)))]
    (tb/round (* (/ rate days-in-month) days-remaining) 2)))


;; ==============================================================================
;; Daily ========================================================================
;; ==============================================================================


;; NOTE: We need to correct for timezones throughout our events. While we're in
;; California only, it's alright to just hardcode this. Obviously, that won't
;; work forever. How will this system work later?
(def tz
  (t/time-zone-for-id "America/Los_Angeles"))


(def reminder-interval
  "The number of days which should pass between sending renewal reminders to an
  unresponsive member."
  [45 40 37 33 31])


(defmethod dispatch/job :ops/daily
  [deps event {:keys [t]}]
  [(event/report ::report-untransitioned-licenses
                 {:params       {:t t}
                  :triggered-by event})
   (event/job ::send-renewal-reminders
              {:params       {:t        t
                              :interval reminder-interval}
               :triggered-by event})
   (event/job ::create-month-to-month-renewals
              {:params       {:t t}
               :triggered-by event})
   (event/job ::cancel-transitioning-orders
              {:params       {:t t}
               :triggered-by event})])


;; helpers ======================================================================


(defn licenses-without-transitions-between
  [db from to]
  (->> (d/q
        '[:find [?l ...]
          :in $ ?start ?end
          :where
          [?l :member-license/ends ?date]
          [?l :member-license/status :member-license.status/active]
          [(>= ?date ?start)]
          [(<= ^java.util.Date ?date ?end)]]
        db from to)
       (map (partial d/entity db))
       (remove member-license/has-transition?)))


(defn licenses-without-transitions-ending-at
  "Find all the licenses that do not have transitions that end on `date`."
  [db date]
  (let [start (date/beginning-of-day date tz)
        end   (date/end-of-day date tz)]
    (licenses-without-transitions-between db start end)))


(defn licenses-without-transitions-ending-in-days
  "Find all the licenses that do not have transitions that end a precise number of
  `days` after date `t`."
  [db t days]
  (let [then  (c/to-date (t/plus (c/to-date-time t) (t/days days)))
        start (date/beginning-of-day then tz)
        end   (date/end-of-day then tz)]
    (licenses-without-transitions-between db start end)))


(defn licenses-without-transitions-ending-within
  "Find all the licenses that do not have transitions that end within `from-days`
  and `to-days` after date `t`."
  [db t days-from days-to]
  (let [from  (c/to-date (t/plus (c/to-date-time t) (t/days days-from)))
        to    (c/to-date (t/plus (c/to-date-time t) (t/days days-to)))
        start (date/beginning-of-day from tz)
        end   (date/end-of-day to tz)]
    (licenses-without-transitions-between db start end)))


;; Report soon-to-expire, untransitioned licenses ===============================


(defn- fmt-license [db t i license]
  (let [account (member-license/account license)
        ends-on (member-license/ends license)
        days    (-> (c/to-date-time t)
                    (t/interval (c/to-date-time ends-on))
                    (t/in-days))]
    (format "%s. %s's license for %s is expiring *in %s days* (on %s)"
            (inc i)
            (account/short-name account)
            (unit/code (member-license/unit license))
            days
            (-> ends-on
                (date/tz-uncorrected (member-license/time-zone license))
                date/short))))


(defn send-untransitioned-report
  [deps t property licenses]
  (slack/send
   (->slack deps)
   {:channel (property/slack-channel property)}
   (sm/msg
    (sm/warn
     (sm/title "The following members have expiring licenses")
     (sm/pretext "_I've sent an email reminding each member to inform us of their plans for the end of their license._")
     (sm/text (->> (sort-by member-license/ends licenses)
                   (map-indexed (partial fmt-license (->db deps) t))
                   (interpose "\n")
                   (apply str)))))))



(defmethod dispatch/report ::report-untransitioned-licenses
  [deps event {:keys [t] :as params}]
  (let [[from to] [(last reminder-interval) (first reminder-interval)]
        licenses  (licenses-without-transitions-ending-within (->db deps) t from to)]
    (doseq [[property licenses] (group-by member-license/property licenses)]
      (send-untransitioned-report deps t property licenses))))


;; Member Renewal Reminders =====================================================


(defmethod dispatch/job ::send-renewal-reminders
  [deps event {:keys [t interval] :as params}]
  (let [licenses (licenses-without-transitions-ending-in-days (->db deps) t (first interval))]
    (-> (map
         (fn [license]
           (event/notify ::send-renewal-reminder {:params       {:license-id (td/id license)
                                                                 :days       (first interval)}
                                                  :triggered-by event}))
         licenses)
        (tb/conj-when
         (when-not (empty? (rest interval))
           (event/job (event/key event) {:params       {:interval (rest interval)
                                                        :t        t}
                                         :triggered-by event}))))))


(def renewal-reminder-pre-anniversary-document-id
  "The Tipe document id for the renewal reminder email template to be sent to
  members that have not yet lived in the same unit for one year"
  "5b196fde154a600013c5757a")


(def renewal-reminder-post-anniversary-document-id
  "The Tipe document id for the renewal reminder email template to be sent to
  members that have lived in the same unit for one year"
  "5b22ea0f8a4d5f0013aad3ad")


(defn get-renewal-reminder-email-document-id
  "Gets the pre- or post-anniversary renewal reminder email template from Tipe.
  NOTE: This logic will miss members who have lived in the same unit for 12
  months by renewing a combination of 3-, 6-, or 1-month terms. Logic to account
  for this case will come in a future development sprint."
  [license]
  (if (= 12 (member-license/term license))
    renewal-reminder-post-anniversary-document-id
    renewal-reminder-pre-anniversary-document-id))


(defn prepare-renewal-email
  [document account license]
  (tb/transform-when-key-exists document
    {:body (fn [body]
             (-> (stache/render body {:name   (account/first-name account)
                                      :ends   (date/short (member-license/ends license))
                                      :sender "Starcity Community"})
                 (md/md-to-html-string)))}))


(defmethod dispatch/notify ::send-renewal-reminder
  [deps event {:keys [license-id days]}]
  (let [license     (d/entity (->db deps) license-id)
        account     (member-license/account license)
        document-id (get-renewal-reminder-email-document-id license)
        document    (tipe/fetch-document (->tipe deps) document-id)
        content     (prepare-renewal-email document account license)]
    (mailer/send
     (->mailer deps)
     (account/email account)
     (mail/subject (:subject content))
     (mm/msg (:body content) (or (:signature content) (mail/noreply-sig)))
     {:uuid (event/uuid event)
      :from (or (:from content) (mail/from-community))
      :bcc  (when (config/production? config) mail/community-address)})))


;; passive renewals (roll to month-to-month) ====================================


(defmethod dispatch/job ::create-month-to-month-transition
  [deps event {:keys [license-id] :as params}]
  (let [old-license (d/entity (->db deps) license-id)
        new-license (member-license/create (license/by-term (->db deps) 1)
                                           (member-license/unit old-license)
                                           (member-license/ends old-license)
                                           (member-license/rate old-license)
                                           :member-license.status/pending)
        transition  (transition/create old-license
                                       :license-transition.type/renewal
                                       (member-license/starts new-license)
                                       {:new-license new-license})]
    [new-license
     transition
     {:db/id            (-> old-license member-license/account td/id)
      :account/licenses (td/id new-license)}
     (events/month-to-month-transition-created transition)]))


(defmethod dispatch/job ::create-month-to-month-renewals
  [deps event {:keys [t] :as params}]
  (let [one-month-out (t/plus (c/to-date-time t) (t/months 1))
        mtm-license   (license/by-term (->db deps) 1)
        licenses      (->> (licenses-without-transitions-ending-at (->db deps) one-month-out)
                           (filter (comp #{1} member-license/term)))]
    (conj
     (map
      (fn [license]
        (event/job ::create-month-to-month-transition
                   {:params       {:license-id (td/id license)}
                    :triggered-by event}))
      licenses)
     (event/job ::deactivate-expired-licenses
                {:params       {:t t}
                 :triggered-by event})
     (event/job ::activate-pending-licenses
                {:params       {:t t}
                 :triggered-by event}))))


;; deactivate old licenses ======================================================


(defn- expired-licenses
  [db as-of]
  (d/q
   '[:find [?l ...]
     :in $ ?as-of
     :where
     [?l :member-license/status :member-license.status/active]
     [?l :member-license/ends ?end-date]
     [(.before ^java.util.Date ?end-date ?as-of)]]
   db as-of))


(defmethod dispatch/job ::deactivate-expired-licenses
  [deps event {:keys [t] :as params}]
  (let [license-ids (expired-licenses (->db deps) t)]
    (map
     (fn [license-id]
       [:db/add license-id :member-license/status :member-license.status/inactive])
     license-ids)))


;; activate pending licenses ====================================================


(defn- pending-licenses
  [db as-of]
  (let [start-of-day (date/beginning-of-day as-of tz)
        end-of-day   (date/end-of-day as-of tz)]
    (d/q
     '[:find [?l ...]
       :in $ ?sod ?eod
       :where
       [?l :member-license/status :member-license.status/pending]
       [?l :member-license/commencement ?start-date]
       [(>= ?start-date ?sod)]
       [(< ?start-date ?eod)]]
     db start-of-day end-of-day)))


(defmethod dispatch/job ::activate-pending-licenses
  [deps event {:keys [t] :as params}]
  (let [license-ids (pending-licenses (->db deps) t)]
    (map
     (fn [license-id]
       [:db/add license-id :member-license/status :member-license.status/active])
     license-ids)))


;; deactivate helping hands subscriptions =======================================


(defn- licenses-transferring-or-moving-out
  "All licenses with move-out or transfer transitions coming up within the next 30
  days."
  [db as-of]
  (let [as-of (c/to-date (t/plus (c/to-date-time as-of) (t/days 30)))]
    (->> (d/q
          '[:find [?l ...]
            :in $ ?as-of
            :where
            [?l :member-license/status :member-license.status/active]
            [?t :license-transition/current-license ?l]
            [?t :license-transition/date ?date]
            [(.before ^java.util.Date ?date ?as-of)]
            (or [?t :license-transition/type :license-transition.type/inter-xfer]
                [?t :license-transition/type :license-transition.type/intra-xfer]
                [?t :license-transition/type :license-transition.type/move-out])]
          db as-of)
         (map (partial d/entity db)))))


(defn- active-order-subs
  [teller license]
  (let [account  (member-license/account license)
        customer (tcustomer/by-account teller account)]
    (tsubscription/query teller {:customers     [customer]
                                 :payment-types [:payment.type/order]})))


(defn- tomorrow
  [date]
  (c/to-date (t/plus (c/to-date-time date) (t/days 1))))


(defn- bills-tomorrow?
  "Will `subscription` bill a day after `date`?"
  [subscription date]
  (let [billing-date (tsubscription/current-billing-date subscription date)
        tomorrow     (tomorrow date)
        from         (date/beginning-of-day tomorrow)
        to           (date/end-of-day tomorrow)]
    (t/within? (c/to-date-time from) (c/to-date-time to) (c/to-date-time billing-date))))


(defmethod dispatch/job ::cancel-transitioning-subs
  [deps event {:keys [t bill-to subscription-id] :as params}]
  (let [subscription (tsubscription/by-id (->teller deps) subscription-id)
        order        (order/by-subscription (->db deps) subscription)
        amount       (prorated-amount (order/computed-price order) (tomorrow t))
        payment      (tpayment/create! (tsubscription/customer subscription) amount :payment.type/order
                                       {:property (tsubscription/property subscription)
                                        :due      bill-to
                                        :period   [(tomorrow t) bill-to]
                                        :status   :payment.status/due})]
    (tsubscription/cancel! subscription)
    {:db/id          (td/id order)
     :order/status   :order.status/canceled
     :order/payments (td/id payment)}))


(defmethod dispatch/job ::cancel-transitioning-orders
  [deps event {:keys [t] :as params}]
  (let [licenses    (licenses-transferring-or-moving-out (->db deps) t)
        active-subs (->> (mapcat (partial active-order-subs (->teller deps)) licenses)
                         (filter #(bills-tomorrow? % t)))]

    (map
     (fn [subs]
       (let [license (->> (tsubscription/customer subs) tcustomer/account (member-license/active (->db deps)))]
         (event/job ::cancel-transitioning-subs {:params       {:t               t
                                                                :bill-to         (member-license/ends license)
                                                                :subscription-id (tsubscription/id subs)}
                                                 :triggered-by event})))
     active-subs)))


;; ==============================================================================
;; First of Month ===============================================================
;; ==============================================================================


(defmethod dispatch/job :ops/first-of-month
  [deps event {:keys [t] :as params}]
  (events/create-monthly-rent-payments t))


;; ==============================================================================
;; End of Month =================================================================
;; ==============================================================================


(defmethod dispatch/job :ops/end-of-month
  [deps event {:keys [t] :as params}]
  (let [t      (c/to-date (t/plus (c/to-date-time t) (t/days 10)))
        period (date/beginning-of-month t)]
    [(event/job ::deactivate-autopay-for-move-outs
                {:params       {:period period}
                 :triggered-by event})
     (event/job ::migrate-transitions
                {:params       {:period period}
                 :triggered-by event})]))


;; helpers ======================================================================


(defmethod dispatch/job ::create-prorated-payment
  [deps event {:keys [transition-id] :as params}]
  (let [transition (d/entity (->db deps) transition-id)
        license    (transition/new-license transition)
        tz         (member-license/time-zone license)
        starts     (member-license/starts license)
        from       (date/beginning-of-day starts tz)
        to         (date/end-of-month starts tz)
        amount     (-> license member-license/rate (prorated-amount from))
        customer   (tcustomer/by-account (->teller deps) (member-license/account license))
        property   (tproperty/by-community (->teller deps) (member-license/property license))]
    (tpayment/create! customer amount :payment.type/rent
                      {:period   [from to]
                       :property property
                       :status   :payment.status/due})
    nil))


;; reactivate autopay ===========================================================


(defn- plan-name
  [teller license]
  (let [account       (member-license/account license)
        email         (account/email account)
        unit-name     (unit/code (member-license/unit license))
        customer      (tcustomer/by-account teller account)
        property      (tcustomer/property customer)
        property-name (tproperty/name property)]
    (str "autopay for " email " @ " property-name " in " unit-name)))


(defn- reactivate-on
  [license]
  (let [tz (member-license/time-zone license)]
    (-> license
        (member-license/starts)
        (c/to-date-time)
        (t/plus (t/months 1))
        (c/to-date)
        (date/beginning-of-month tz))))


(defn- create-plan!
  [teller license]
  (tplan/create! teller
                 (plan-name teller license)
                 :payment.type/rent
                 (member-license/rate license)))


(defmethod dispatch/job ::reactivate-autopay
  [deps event {:keys [transition-id source-id] :as params}]
  (let [transition (d/entity (->db deps) transition-id)
        license    (transition/new-license transition)
        customer   (tcustomer/by-account (->teller deps)
                                         (member-license/account license))
        plan       (create-plan! (->teller deps) license)]
    (tsubscription/subscribe! customer plan {:source   (tsource/by-id customer source-id)
                                             :start-at (reactivate-on license)})
    nil))


;; deactivate autopay ===========================================================


(defn- autopay-subscription [teller customer]
  (first (tsubscription/query teller {:customers     [customer]
                                      :payment-types [:payment.type/rent]
                                      :canceled      false})))


(defn- autopay-on?
  [teller license]
  (let [account  (member-license/account license)
        customer (tcustomer/by-account teller account)]
    (some? (autopay-subscription teller customer))))


(defn- deactivate-autopay-events
  [triggered-by licenses]
  (map
   (fn [license]
     (let [transition (first (:license-transition/_current-license license))]
       (event/job ::deactivate-autopay {:params       {:transition-id (td/id transition)
                                                       :reactivate?   false}
                                        :triggered-by triggered-by})))
   licenses))


(defmethod dispatch/job ::deactivate-autopay
  [deps event {:keys [transition-id reactivate?] :as params}]
  (let [transition (d/entity (->db deps) transition-id)
        license    (transition/current-license transition)
        account    (member-license/account license)
        customer   (tcustomer/by-account (->teller deps) account)
        sub        (autopay-subscription (->teller deps) customer)
        source-id  (tsource/id (tsubscription/source sub))]
    (tsubscription/cancel! sub)
    (when reactivate?
      (event/job ::reactivate-autopay {:params       {:transition-id transition-id
                                                      :source-id     source-id}
                                       :triggered-by event}))))


;; moveouts =====================================================================


(defn- is-transitioning-within-month? [period transition]
  (let [ends    (transition/date transition)
        license (transition/current-license transition)
        tz      (member-license/time-zone license)
        from    (date/beginning-of-month period tz)
        to      (date/end-of-month period tz)]
    (t/within? (c/to-date-time from) (c/to-date-time to) (c/to-date-time ends))))


(defmethod dispatch/job ::deactivate-autopay-for-move-outs
  [deps event {:keys [period] :as params}]
  (->> (transition/by-type (->db deps) :license-transition.type/move-out)
       (filter (partial is-transitioning-within-month? period))
       (map transition/current-license)
       (filter (partial autopay-on? (->teller deps)))
       (deactivate-autopay-events event)))


;; migrate transition ===========================================================


(defn- ends-after-first-of-month? [period transition]
  (let [license       (transition/current-license transition)
        tz            (member-license/time-zone license)
        transitioning (transition/date transition)
        from          (-> (date/beginning-of-month period tz)
                          (c/to-date-time)
                          (t/plus (t/days 1))
                          (c/to-date))
        to            (date/end-of-month period tz)]
    (t/within? (c/to-date-time from) (c/to-date-time to) (c/to-date-time transitioning))))


(defn- has-new-rate? [transition]
  (not=
   (member-license/rate (transition/current-license transition))
   (member-license/rate (transition/new-license transition))))


(defmethod dispatch/job ::migrate-transition
  [deps event {:keys [period transition-id] :as params}]
  (let [transition (d/entity (->db deps) transition-id)]
    (cond-> []
      (and (has-new-rate? transition)
           (autopay-on? (->teller deps) (transition/current-license transition)))
      (conj (event/job ::deactivate-autopay
                       {:params       {:transition-id (td/id transition)
                                       :reactivate?   true}
                        :triggered-by event}))

      ;; Only create a second-half prorated payment if we're dealing with an
      ;; inter-transfer (new building) or a rate change
      (and (ends-after-first-of-month? period transition)
           (or (= (transition/type transition) :license-transition.type/inter-xfer)
               (has-new-rate? transition)))
      (conj (event/job ::create-prorated-payment
                       {:params       {:transition-id (td/id transition)}
                        :triggered-by event})))))


;; migrate transitions ==========================================================


(defmethod dispatch/job ::migrate-transitions
  [deps event {:keys [period] :as params}]
  (let [transitions (concat
                     (transition/by-type (->db deps) :license-transition.type/renewal)
                     (transition/by-type (->db deps) :license-transition.type/inter-xfer)
                     (transition/by-type (->db deps) :license-transition.type/intra-xfer))]
    (map
     (fn [transition]
       (event/job ::migrate-transition {:params       {:period        period
                                                       :transition-id (td/id transition)}
                                        :triggered-by event}))
     transitions)))
