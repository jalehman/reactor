(ns reactor.handlers.operations
  (:require [blueprints.models.account :as account]
            [blueprints.models.event :as event]
            [blueprints.models.events :as events]
            [blueprints.models.license :as license]
            [blueprints.models.license-transition :as transition]
            [blueprints.models.member-license :as member-license]
            [blueprints.models.unit :as unit]
            [clj-time.coerce :as c]
            [clj-time.core :as t]
            [clostache.parser :as stache]
            [datomic.api :as d]
            [mailer.core :as mailer]
            [mailer.message :as mm]
            [markdown.core :as md]
            [reactor.dispatch :as dispatch]
            [reactor.handlers.common :refer :all]
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
;; Daily ========================================================================
;; ==============================================================================


(def reminder-interval
  "The number of days which should pass between sending renewal reminders to an
  unresponsive member."
  [45 40 37 33 31])


(defmethod dispatch/job :ops/daily
  [deps event params]
  ;;TODO - dispatch an event that will
  ;;   1. look for licenses ending on that day
  ;;   2. deactivate those licenses
  ;;   3. if any of the licenses have a "next license" attr, create/activate those new licenses
  )





;; helpers ======================================================================


(defn- licenses-without-transitions-ending-in-days
  "Find all the licenses that do not have transitions that end a precise number of
  `days` after date `t`."
  [db t days]
  (let [then  (c/to-date (t/plus (c/to-date-time t) (t/days days)))
        start (c/to-date (date/beginning-of-day then))
        end   (c/to-date (date/end-of-day then))]
    (->> (d/q
          '[:find [?l ...]
            :in $ ?start ?end
            :where
            [?l :member-license/ends ?date]
            [(.after ^java.util.Date ?date ?start)]
            [(.before ^java.util.Date ?date ?end)]]
          db start end)
         (map (partial d/entity db))
         (filter member-license/has-transition?))))


;; Renewal Reminders ============================================================


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


(def renewal-reminder-document-id
  "the Tipe document id for the renewal reminder email template"
  "5b196fde154a600013c5757a")


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
  (let [license  (d/entity (->db deps) license-id)
        account  (member-license/account license)
        document (tipe/fetch-document (->tipe deps) renewal-reminder-document-id)
        content  (prepare-renewal-email document account license)]
    (mailer/send
     (->mailer deps)
     (account/email account)
     (mail/subject (:subject content))
     (mm/msg (:body content))
     {:uuid (event/uuid event)})))


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
     (events/month-to-month-transition-created transition)]))


(defmethod dispatch/job ::create-month-to-month-renewals
  [deps event {:keys [t] :as params}]
  (let [licenses (licenses-without-transitions-ending-in-days (->db deps) t 30)]
    (map
     (fn [license]
       (event/job ::create-month-to-month-transition {:params {:license-id (td/id license)}
                                                      :triggered-by      event}))
     licenses)))


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


;; ==============================================================================
;; First of Month ===============================================================
;; ==============================================================================


(defmethod dispatch/job :ops/first-of-month
  [deps event {:keys [t] :as params}]
  (event/job ::passive-renewals {:params       {:t t}
                                 :triggered-by event}))


;; ==============================================================================
;; End of Month =================================================================
;; ==============================================================================


(defmethod dispatch/job :ops/end-of-month
  [deps event {:keys [t] :as params}]
  (let [t      (c/to-date (t/plus (c/to-date-time t) (t/days 10)))
        period (date/beginning-of-month t)]
    [(event/job ::deactivate-autopay-for-move-outs {:params       {:period period}
                                                    :triggered-by event})
     (event/job ::prepare-renewals {:params       {:period period}
                                    :triggered-by event})]))


;; helpers ======================================================================


(defn- prorated-amount
  [rate starts]
  (let [starts         (c/to-date-time starts)
        days-in-month  (t/day (t/last-day-of-the-month starts))
        days-remaining (inc (- days-in-month (t/day starts)))]
    (tb/round (* (/ rate days-in-month) days-remaining) 2)))


(defmethod dispatch/job ::create-prorated-payment
  [deps event {:keys [transition-id] :as params}]
  (let [transition (d/entity (->db deps) transition-id)
        license    (transition/new-license transition)
        starts     (member-license/starts license)
        tz         (member-license/time-zone license)
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


;; moveouts =============================


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


;; prepare renewals =============================================================


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


(defmethod dispatch/job ::prepare-renewal
  [deps event {:keys [period transition-id] :as params}]
  (let [transition (d/entity (->db deps) transition-id)]
    (cond-> []
      (and (has-new-rate? transition)
           (autopay-on? (->teller deps) (transition/current-license transition)))
      (conj (event/job ::deactivate-autopay
                       {:params       {:transition-id (td/id transition)
                                       :reactivate?   true}
                        :triggered-by event}))

      (ends-after-first-of-month? period transition)
      (conj (event/job ::create-prorated-payment
                       {:params       {:transition-id (td/id transition)}
                        :triggered-by event})))))


(defmethod dispatch/job ::prepare-renewals
  [deps event {:keys [period] :as params}]
  (->> (transition/by-type (->db deps) :license-transition.type/renewal)
       (map
        (fn [transition]
          (event/job ::prepare-renewal {:params       {:period        period
                                                       :transition-id (td/id transition)}
                                        :triggered-by event})))))





(defn- find-passive-renewal-licenses
  [db t]
  (d/q '[:find ?l
         :in $ ?month
         :where
         [?l :member-license/ends ?term-end]
         [(t/before? ?term-end (date/end-of-month ?month))]
         [(t/after? ?term-end (date/beginning-of-month ?month))]
         [(missing? $ ?l :member-license/transition)]]
       db t))


(defmethod dispatch/job ::passive-renewals
  [deps event {:keys [t] :as params}]
  (event/job ::active-renewals {:params                 {:t t}
                                :triggered-by event}))

(defn- find-active-renewal-licenses
  [db t]
  (d/q '[:find ?l
         :in $ ?month
         :where
         [?l :member-license/ends ?term-end]
         [(t/before? ?term-end (date/end-of-month ?month))]
         [(t/after? ?term-end (date/beginning-of-month ?month))]
         [?l :member-license/transition ?transition]
         [?transition :license-transition/type :license-transition.type/move-out]]
       db t))



(defmethod dispatch/job ::active-renewals
  [deps event {:keys [t] :as params}]
  (events/create-monthly-rent-payments t))


;; Deactivate Old Licenses ======================================================
(defn- licenses-that-have-ended-before
  [db date]
  (->> (d/q '[:find [?l ...]
              :in $ ?date
              :where
              [?l :member-license/ends ?ends]
              [(.after ^java.util.Date ?date ?ends)]]
            db (c/to-date date))
       (map (partial d/entity db))))
