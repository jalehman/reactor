(defproject starcity/reactor "0.8.2"
  :description "Transactional event processing queue."
  :url "https://github.com/starcity-properties/reactor"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha14"]
                 [org.clojure/core.async "0.3.443"]
                 [org.clojure/tools.nrepl "0.2.12"]
                 [org.clojure/tools.cli "0.3.5"]
                 ;; db
                 [starcity/blueprints "1.16.0" :exclusions [com.datomic/datomic-free]]
                 ;; services
                 [starcity/ribbon "0.7.0"]
                 [starcity/mailer "0.1.0"]
                 ;; util
                 [im.chit/hara.io.scheduler "2.5.10"]
                 [starcity/drawknife "0.2.0"]
                 [starcity/customs "0.1.0"]
                 [clj-time "0.13.0"]
                 [mount "0.1.11"]
                 [starcity/toolbelt "0.1.11" :exclusions [com.datomic/datomic-free]]
                 [com.taoensso/timbre "4.10.0"]
                 [aero "1.1.2"]
                 [ring "1.6.1"]
                 ;; dependency resolution
                 [org.apache.httpcomponents/httpclient "4.5.3"]]

  :plugins [[s3-wagon-private "1.2.0"]]

  :repositories {"releases" {:url        "s3://starjars/releases"
                             :username   :env/aws_access_key
                             :passphrase :env/aws_secret_key}}

  :repl-options {:init-ns user}

  :main reactor.core)
