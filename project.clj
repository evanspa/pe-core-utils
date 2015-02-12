(defproject pe-core-utils "0.0.1-SNAPSHOT"
  :description "A Clojure library providing a set of helper functions."
  :url "https://github.com/evanspa/pe-core-utils"
  :license {:name "MIT"
            :url "http://opensource.org/licenses/MIT"}
  :plugins [[lein-pprint "1.1.2"]
            [lein-marginalia "0.8.0"]
            [codox "0.8.10"]]
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/tools.logging "0.3.1"]
                 [ch.qos.logback/logback-classic "1.0.13"]
                 [org.slf4j/slf4j-api "1.7.5"]
                 [org.clojure/tools.nrepl "0.2.7"]]
  :resource-paths ["resources"]
  :profiles {:dev {:source-paths ["dev"]  ;ensures 'user.clj' gets auto-loaded
                   :plugins [[cider/cider-nrepl "0.9.0-SNAPSHOT"]]
                   :dependencies [[org.clojure/tools.namespace "0.2.7"]
                                  [org.clojure/java.classpath "0.2.2"]]}}
  :repositories [["releases" {:url "https://clojars.org/repo"
                              :creds :gpg}]])