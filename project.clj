(defproject pe-core-utils "0.0.11-SNAPSHOT"
  :description "A Clojure library providing a set of helper functions."
  :url "https://github.com/evanspa/pe-core-utils"
  :license {:name "MIT"
            :url "http://opensource.org/licenses/MIT"}
  :plugins [[lein-pprint "1.1.2"]
            [codox "0.8.10"]]
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/tools.logging "0.3.1"]
                 [ch.qos.logback/logback-classic "1.0.13"]
                 [clj-time "0.8.0"]
                 [org.slf4j/slf4j-api "1.7.5"]]
  :resource-paths ["resources"]
  :codox {:exclude [user]
          :src-dir-uri "https://github.com/evanspa/pe-core-utils/blob/0.0.11/"
          :src-linenum-anchor-prefix "L"}
  :profiles {:dev {:source-paths ["dev"]  ;ensures 'user.clj' gets auto-loaded
                   :plugins [[cider/cider-nrepl "0.9.0-SNAPSHOT"]]
                   :dependencies [[org.clojure/tools.namespace "0.2.7"]
                                  [org.clojure/java.classpath "0.2.2"]
                                  [org.clojure/tools.nrepl "0.2.7"]]}}
  :repositories [["releases" {:url "https://clojars.org/repo"
                              :creds :gpg}]])
