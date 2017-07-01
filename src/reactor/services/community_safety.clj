(ns reactor.services.community-safety
  (:require [cheshire.core :as json]
            [clj-time
             [coerce :as c]
             [core :as t]]
            [clojure
             [spec :as s]
             [string :as string]]
            [org.httpkit.client :as http]
            [toolbelt.core :as tb]
            [clojure.core.async :as a]))

;; =============================================================================
;; Helpers
;; =============================================================================


(def ^:private base-uri
  "https://api.communitysafety.goodhire.com/v1/%s")


(defn- cb
  [c {:keys [body headers status] :as res}]
  (let [body (json/parse-string body true)]
    (if (#{201 200} status)
     (a/put! c (assoc res :body body))
     (a/put! c (ex-info (:Message body) body)))))


(defn- community-safety-request
  [api-key endpoint params]
  (let [c (a/chan 1)]
    (http/post (format base-uri endpoint)
               {:body    (json/generate-string params)
                :headers {"Content-Type"  "application/json"
                          "Authorization" (format "ApiKey %s" api-key)}}
               (partial cb c))
    c))

(s/fdef community-safety-request
        :args (s/cat :api-key string?
                     :endpoint string?
                     :params map?)
        :ret map?)


(defn- dob-params [dob]
  (let [dt (c/from-date dob)]
    {:BirthYear  (t/year dt)
     :BirthMonth (t/month dt)
     :BirthDay   (t/day dt)}))


;; =============================================================================
;; Selectors
;; =============================================================================


(defn report-url
  "The URL of the background check report."
  [response]
  (get-in response [:headers :location]))


;; =============================================================================
;; API
;; =============================================================================


(defprotocol ICommunitySafety
  "Interface to communicate with the Community Safety API."
  (background-check
    [this user-id first-name last-name email dob]
    [this user-id first-name last-name email dob opts]
    "Perform a background check."))


(defrecord CommunitySafety [api-key])


(s/def ::non-empty-string (comp not string/blank?))
(s/def ::middle-name ::non-empty-string)
(s/def ::city ::non-empty-string)
(s/def ::state ::non-empty-string)
(s/def ::postal-code ::non-empty-string)
(s/def ::address
  (s/keys :req-un [::city ::state ::postal-code]))
(s/def ::opts
  (s/keys :opt-un [::middle-name ::address]))


(defn- background-check*
  "Initiate a background check via the Community Safety API."
  [api-key user-id first-name last-name email dob {:keys [middle-name address]}]
  (let [{:keys [city state postal-code]} address]
    (community-safety-request api-key
                              "Profile"
                              (-> {:UserID    (str user-id)
                                   :FirstName first-name
                                   :LastName  last-name
                                   :Email     email}
                                  (merge (dob-params dob))
                                  (tb/assoc-when
                                   :MiddleName middle-name
                                   :City city
                                   :State state
                                   :ZipCode postal-code)))))

(s/fdef background-check*
        :args (s/cat :api-key string?
                     :user-id integer?
                     :first-name ::non-empty-string
                     :last-name ::non-empty-string
                     :email ::non-empty-string
                     :dob inst?
                     :opts ::opts))


(extend-protocol ICommunitySafety
  CommunitySafety
  (background-check
    ([this user-id first-name last-name email dob]
     (background-check this user-id first-name last-name email dob {}))
    ([this user-id first-name last-name email dob opts]
     (background-check* (:api-key this) user-id first-name last-name email dob opts))))


(defn community-safety [api-key]
  (map->CommunitySafety {:api-key api-key}))
