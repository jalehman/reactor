(defproject starcity/reactor "1.7.4-SNAPSHOT"
  :description "Transactional event processing queue."
  :url "https://github.com/starcity-properties/reactor"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/core.async "0.4.474"]
                 [org.clojure/tools.nrepl "0.2.13"]
                 [org.clojure/tools.cli "0.3.5"]
                 ;; db
                 [starcity/blueprints "2.4.0-SNAPSHOT" :exclusions [com.datomic/datomic-free]]
                 ;; services
                 [starcity/ribbon "0.11.0"]
                 [starcity/mailer "0.2.0"]
                 [starcity/hubspot-clj "0.2.0"]
                 ;; util
                 [hiccup "1.0.5"]
                 [im.chit/hara.io.scheduler "2.5.10"]
                 [starcity/drawknife "1.0.0"]
                 [starcity/customs "1.0.0"]
                 [starcity/toolbelt-async "0.4.0"]
                 [starcity/toolbelt-core "0.3.0"]
                 [starcity/toolbelt-date "0.3.0"]
                 [starcity/toolbelt-datomic "0.5.0"]
                 [com.taoensso/timbre "4.10.0"]
                 [clj-time "0.14.2"]
                 [mount "0.1.11"]
                 [aero "1.1.2"]
                 [ring "1.6.3"]
                 ;; dependency resolution
                 [org.apache.httpcomponents/httpclient "4.5.4"]]

  :plugins [[s3-wagon-private "1.2.0"]]

  :repositories {"releases" {:url        "s3://starjars/releases"
                             :username   :env/aws_access_key
                             :passphrase :env/aws_secret_key}}

  :repl-options {:init-ns user}

  :main reactor.core)
