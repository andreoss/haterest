(defproject hypermedia "0.1.0"
  :description "A hypermedia API derived from a schema"
  :dependencies [[org.clojure/clojure "1.12.0"]
                 [org.clojure/tools.cli "1.1.230"]
                 [metosin/malli "0.16.4"]
                 [metosin/jsonista "0.3.13"]
                 [metosin/reitit-ring "0.7.2"]
                 [ring/ring-core "1.13.0"]
                 [ring/ring-jetty-adapter "1.13.0"]
                 [com.github.seancorfield/next.jdbc "1.3.955"]
                 [com.zaxxer/HikariCP "6.2.1"]
                 [org.slf4j/slf4j-api "2.0.16"]]

  :source-paths ["src"]
  :resource-paths ["resources"]
  :test-paths ["test"]
  :target-path "target/%s"
  :main hypermedia.main

  :profiles
  {:drivers  {:dependencies [[com.h2database/h2 "2.3.232" :scope "runtime"]
                             [org.xerial/sqlite-jdbc "3.46.1.3" :scope "runtime"]
                             [org.hsqldb/hsqldb "2.7.3" :scope "runtime"]
                             [org.apache.derby/derby "10.17.1.0" :scope "runtime"]
                             [org.apache.derby/derbytools "10.17.1.0" :scope "runtime"]
                             [org.apache.derby/derbyshared "10.17.1.0" :scope "runtime"]
                             [org.postgresql/postgresql "42.7.4" :scope "runtime"]
                             [org.mariadb.jdbc/mariadb-java-client "3.4.1" :scope "runtime"]
                             [org.slf4j/slf4j-nop "2.0.16" :scope "runtime"]]}

   :tests    {:source-paths ["test"]
              :jvm-opts ["-Djdk.httpclient.allowRestrictedHeaders=connection"]}

   :kaocha   {:dependencies [[lambdaisland/kaocha "1.91.1392"]]}

   :cover    {:dependencies [[cloverage "1.2.4"]]}

   :sources  {:source-paths ["dev" "test"]}

   :compiled {:aot :all
              :uberjar-name "hypermedia-standalone.jar"}

   :harness  [:drivers :tests :kaocha]

   :coverage [:drivers :tests :cover]

   :dev      [:drivers :sources]

   :uberjar  {:dependencies [[com.h2database/h2 "2.3.232"]
                             [org.xerial/sqlite-jdbc "3.46.1.3"]
                             [org.hsqldb/hsqldb "2.7.3"]
                             [org.apache.derby/derby "10.17.1.0"]
                             [org.apache.derby/derbytools "10.17.1.0"]
                             [org.apache.derby/derbyshared "10.17.1.0"]
                             [org.postgresql/postgresql "42.7.4"]
                             [org.mariadb.jdbc/mariadb-java-client "3.4.1"]
                             [org.slf4j/slf4j-nop "2.0.16"]]
              :aot :all
              :uberjar-name "hypermedia-standalone.jar"}}

  :aliases
  {"test"    ["with-profile" "+harness" "run" "-m" "kaocha.runner" ":unit"]
   "e2e"     ["with-profile" "+harness" "run" "-m" "kaocha.runner" ":e2e" "--no-capture-output"]
   "suite"   ["with-profile" "+harness" "run" "-m" "kaocha.runner"]
   "cover"   ["with-profile" "+coverage" "run" "-m" "cloverage.coverage"
              "-p" "src" "-s" "test"
              "-t" "hypermedia\\.(?!.*(e2e|concurrency|package|evolve)).*-test"
              "--fail-threshold" "85" "--codecov"]
   "serve"   ["with-profile" "+drivers" "run" "-m" "hypermedia.main"]
   "bench"   ["with-profile" "+harness" "run" "-m" "hypermedia.bench"]})
