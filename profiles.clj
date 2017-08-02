{:dev     {:source-paths ["src" "env/dev"]
           :dependences  [[com.datomic/datomic-free "0.9.5544"]]}
 :uberjar {:aot          :all
           :main         reactor.core
           :source-paths ["src"]
           :dependencies [[com.datomic/datomic-pro "0.9.5544"]
                          [org.postgresql/postgresql "9.4.1211"]]
           :repositories {"my.datomic.com" {:url      "https://my.datomic.com/repo"
                                            :username :env/datomic_username
                                            :password :env/datomic_password}}}}
