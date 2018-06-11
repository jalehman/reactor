(ns reactor.utils.tipe
  (:require [org.httpkit.client :as http]
            [cheshire.core :as json]))


;;NOTE - let's modularize this some day
(def base-uri
  "The base URI of Tipe."
  "http://api.tipe.io/api/v1/")


(defn parse-document
  [document]
  (->> document
       (:blocks document)
       (reduce (fn [acc {:keys [apiId value]}]
                 (assoc acc (keyword apiId) value))
               {})))


(defn fetch-document
  [tipe document-id]
  (-> @(http/get (str base-uri "document/" document-id)
                 {:headers {"Content-Type"  "application/json"
                            "Authorization" (:api-key tipe)
                            "Tipe-Id"       (:org-secret tipe)}})
      :body
      (json/parse-string true)
      (parse-document)))
