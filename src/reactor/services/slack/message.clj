(ns reactor.services.slack.message
  (:require [clj-time.coerce :as c]
            [toolbelt.core :as tb]))

(defn msg
  "Construct a message to be sent over Slack."
  [& attachments]
  (if-let [text (when (string? (first attachments)) (first attachments))]
    {:attachments (rest attachments)
     :text        text}
    {:attachments attachments}))

(defn- inject-markdown [attachment]
  (let [candidates #{:text :pretext :title}]
    (->> (reduce
          (fn [acc [k _]]
            (if (candidates k) (conj acc k) acc))
          []
          attachment)
         (map name)
         (assoc attachment :mrkdwn_in))))

(defn attachment [& parts]
  (let [a (-> (apply merge parts)
              (inject-markdown))]
    (if (contains? a :fallback)
      a
      (assoc a :fallback "oops...no fallback"))))

;; Parts

(defn text [t] {:text t :fallback t})
(defn pretext [t] {:pretext t})
(defn fallback [t] {:fallback t})
(defn color [c] {:color c})

(defn author
  ([name]
   {:author_name name})
  ([name link & [icon]]
   (tb/assoc-when
    {:author_name name
     :author_link link}
    :author_icon icon)))

(defn title
  ([title]
   {:title title})
  ([title link]
   {:title title :title_link link}))

(defn fields [& fields]
  {:fields (remove nil? fields)})

(defn field [title value & [short]]
  {:title title :value value :short (boolean short)})

(defn image [url & [thumb-url]]
  (tb/assoc-when {:image_url url} :thumb_url thumb-url))

(defn link [url text]
  (format "<%s|%s>" url text))

(defn footer [text & [icon]]
  (tb/assoc-when {:footer text} :footer_icon icon))

(defn timestamp [t] {:ts (c/to-epoch t)})

;; Colors
(def green (color "#00d1b2"))
(def red (color "#f00"))
(def blue (color "#108ee9"))

;; Attachment Templates
(def success (partial attachment green))
(def failure (partial attachment red))
(def info (partial attachment blue))
