(ns reactor.deps
  (:require [clojure.spec.alpha :as s]
            [reactor.services.community-safety :as cs]
            [mailer.core :as mailer]
            [reactor.config :as config :refer [config]]
            [reactor.services.slack :as slack]
            [teller.core :as teller]
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
(s/def ::teller teller/connection?)
(s/def ::public-hostname string?)
(s/def ::dashboard-hostname string?)
(s/def ::deps
  (s/keys :req-un [::mailer
                   ::community-safety
                   ::slack
                   ::teller
                   ::public-hostname
                   ::dashboard-hostname]))


(defn deps
  "Construct the dependencies map for `reactor` to function. When the no-arg
  variant is used mock dependencies will be used."
  ([teller]
   {:community-safety   (mock/community-safety)
    :mailer             (mock/mailer)
    :slack              (mock/slack)
    :teller             teller
    :public-hostname    "http://localhost:8080"
    :dashboard-hostname "http://localhost:8082"})
  ([teller config]
   (s/assert ::config config)
   {:community-safety   (community-safety (:community-safety config))
    :mailer             (mailer (:mailer config))
    :slack              (slack (:slack config))
    :tipe               (:tipe config)
    :teller             teller
    :public-hostname    (:public-hostname config)
    :dashboard-hostname (:dashboard-hostname config)}))

(s/fdef deps
        :args (s/cat :teller ::teller
                     :config (s/? ::config))
        :ret ::deps)


(defn deps? [x]
  (s/valid? ::deps x))
