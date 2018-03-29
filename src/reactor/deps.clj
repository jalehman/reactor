(ns reactor.deps
  (:require [clojure.spec.alpha :as s]
            [ribbon.core :as ribbon]
            [reactor.services.community-safety :as cs]
            [mailer.core :as mailer]
            [reactor.config :as config :refer [config]]
            [reactor.services.slack :as slack]
            [ribbon.core :as ribbon]
            [mock.mock :as mock]
            [toolbelt.core :as tb]))


;; =============================================================================
;; Helpers
;; =============================================================================


;; =====================================
;; Service Constructors

(defn- community-safety [{api-key :api-key}]
  (if (some? api-key)
    (cs/community-safety api-key)
    (mock/community-safety)))


(defn- mailer [{:keys [api-key domain sender send-to] :as conf}]
  (mailer/mailgun api-key domain (tb/assoc-when
                                  {}
                                  :sender sender
                                  :send-to send-to)))


(defn- slack [{:keys [webhook-url username channel]}]
  (if (some? channel)
    (slack/slack webhook-url username channel)
    (slack/slack webhook-url username)))


(defn- stripe [{:keys [secret-key]}]
  (if (some? secret-key)
    (ribbon/stripe-connection secret-key)
    (mock/stripe)))


;; =====================================
;; Config Spec

(s/def :config/mailer
  (s/keys :req-un [:mailer/api-key :mailer/domain]
          :opt-un [:mailer/sender :mailer/send-to]))

(s/def :config/slack
  (s/keys :req-un [:slack/webhook-url :slack/username]
          :opt-un [:slack/channel]))

(s/def :config/community-safety
  (s/keys :req-un [:community-safety/api-key]))

(s/def :config/stripe
  (s/keys :req-un [:stripe/secret-key]))

(s/def ::config
  (s/keys :req-un [:config/mailer
                   :config/slack
                   ::public-hostname
                   ::dashboard-hostname]
          :opt-un [:config/community-safety
                   :config/stripe]))


(defn config? [x]
  (s/valid? ::config x))


;; =============================================================================
;; Deps
;; =============================================================================


(s/def ::mailer #(satisfies? mailer.core/Mailer %))
(s/def ::community-safety #(satisfies? reactor.services.community-safety/ICommunitySafety %))
(s/def ::slack #(satisfies? reactor.services.slack/ISlack %))
(s/def ::stripe ribbon/conn?)
(s/def ::public-hostname string?)
(s/def ::dashboard-hostname string?)
(s/def ::deps
  (s/keys :req-un [::mailer
                   ::community-safety
                   ::slack
                   ::stripe
                   ::public-hostname
                   ::dashboard-hostname]))


(defn deps
  "Construct the dependencies map for `reactor` to function. When the no-arg
  variant is used mock dependencies will be used."
  ([]
   {:community-safety   (mock/community-safety)
    :mailer             (mock/mailer)
    :slack              (mock/slack)
    :stripe             (mock/stripe)
    :public-hostname    "http://localhost:8080"
    :dashboard-hostname "http://localhost:8082"})
  ([config]
   (s/assert ::config config)
   {:community-safety   (community-safety (:community-safety config))
    :mailer             (mailer (:mailer config))
    :slack              (slack (:slack config))
    :stripe             (stripe (:stripe config))
    :public-hostname    (:public-hostname config)
    :dashboard-hostname (:dashboard-hostname config)}))

(s/fdef deps
        :args (s/cat :config ::config)
        :ret ::deps)


(defn deps? [x]
  (s/valid? ::deps x))
