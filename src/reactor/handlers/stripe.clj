(ns reactor.handlers.stripe
  "Only services to `require` the Stripe handlers distributed across multiple
  namespaces."
  (:require [clojure
             [spec :as s]
             [string :as string]]
            [reactor.handlers.stripe.charge]))


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
