(ns reactor.handlers.order
  (:require [blueprints.models.account :as account]
            [blueprints.models.event :as event]
            [blueprints.models.member-license :as member-license]
            [blueprints.models.order :as order]
            [blueprints.models.property :as property]
            [blueprints.models.service :as service]
            [blueprints.models.source :as source]
            [datomic.api :as d]
            [mailer.core :as mailer]
            [mailer.message :as mm]
            [reactor.dispatch :as dispatch]
            [reactor.handlers.common :refer :all]
            [reactor.services.slack :as slack]
            [reactor.services.slack.message :as sm]
            [reactor.utils.mail :as mail]
            [toolbelt.date :as date]
            [toolbelt.datomic :as td]))

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
        :channel (slack/helping-hands)}
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
      :channel (slack/helping-hands)}
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
