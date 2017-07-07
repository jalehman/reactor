(ns reactor.deps
  (:require [clojure.spec :as s]
            [ribbon.core :as ribbon]
            [reactor.services.community-safety :as cs]
            [mailer.core :as mailer]
            [reactor.services.slack :as slack]
            [reactor.services.weebly :as weebly]))


;; =============================================================================
;; Spec
;; =============================================================================


(s/def ::mailer #(satisfies? mailer.core/Mailer %))
(s/def ::community-safety #(satisfies? reactor.services.community-safety/ICommunitySafety %))
(s/def ::slack #(satisfies? reactor.services.slack/ISlack %))
(s/def ::weebly #(satisfies? reactor.services.weebly/WeeblyPromote %))
(s/def ::stripe ribbon/conn?)
(s/def ::public-hostname string?)
(s/def ::deps
  (s/keys :req-un [::mailer
                   ::community-safety
                   ::slack
                   ::weebly
                   ::stripe
                   ::public-hostname]))


;; =============================================================================
;; Constructor
;; =============================================================================


(defn deps
  "Construct the dependencies map for Reactor to function."
  [community-safety mailer slack weebly stripe public-hostname]
  {:community-safety community-safety
   :mailer           mailer
   :slack            slack
   :weebly           weebly
   :stripe           stripe
   :public-hostname  public-hostname})

(s/fdef deps
        :args (s/cat :community-safety ::community-safety
                     :mailer ::mailer
                     :slack ::slack
                     :weebly ::weebly
                     :stripe ::stripe
                     :public-hostname ::public-hostname)
        :ret ::deps)


(defn deps? [x]
  (s/valid? ::deps x))


;; =============================================================================
;; Convenience
;; =============================================================================


(comment
  ;; NOTE: yuck
  (defn production
   [community-safety-api-key mailgun-api-key mailgun-domain slack-webhook-url
    slack-username weebly-site-id weebly-form-id stripe-secret-key hostname]
   (deps (cs/community-safety community-safety-api-key)
         (mailer/mailgun mailgun-api-key mailgun-domain)
         (slack/slack slack-webhook-url slack-username)
         (weebly/weebly weebly-site-id weebly-form-id)
         (ribbon/stripe-connection stripe-secret-key)
         hostname)))
