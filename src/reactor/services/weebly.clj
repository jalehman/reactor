(ns reactor.services.weebly
  (:require [clojure.core.async :refer [chan put! close!]]
            [org.httpkit.client :as http]
            [cheshire.core :as json]
            [clojure.core.async :as a]
            [clojure.spec :as s]))


(defn- cb [c]
  (fn [{body :body}]
    (let [body (json/parse-string body true)]
      (if-let [e (:error body)]
        (put! c (ex-info "Error in request!" body))
        (put! c body))
      (close! c))))

(defn- add-subscriber!
  "Add `email` to our newsletter using the Weebly Promote API.
  NOTE: This is NOT a public api, and is likely to break at some point."
  [site-id form-id email]
  (let [c (chan 1)]
    (http/post (format "https://promote.weebly.com/site/%s/leadForm/%s/lead" site-id form-id)
               {:headers {"Accept"       "application/json"
                          "Content-Type" "application/json"}
                :body    (json/encode {:email   email
                                       :form_id form-id
                                       :optIn   false
                                       :site_id site-id})}
               (cb c))
    c))


(defprotocol WeeblyPromote
  "Interface to communicate with the Weebly Promote API."
  (subscribe! [weebly email]
    "Subscribe `email` to the Weebly newsletter."))


(defrecord Weebly [site-id form-id]
  WeeblyPromote
  (subscribe! [_ email]
    (add-subscriber! site-id form-id email)))


(defn weebly [site-id form-id]
  (map->Weebly {:site-id site-id :form-id form-id}))


(comment
  (subscribe! (map->WeeblyDummy {}) "test@test.com")

  (subscribe! (reify WeeblyPromote
                (subscribe! [_ email]
                  (str "This is reified! " email)))
              "test@test.com")

  (a/<!! (subscribe! (map->Weebly {:site-id "abcd" :form-id "efgh"}) "test@test.com"))


  )
