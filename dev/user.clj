(ns user
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer (pprint)]
            [clojure.repl :refer :all]
            [clojure.stacktrace :refer (e)]
            [clojure.test :as test]
            [pe-core-utils.core :as core]
            [clojure.java.io :refer [resource]]
            [clojure.tools.namespace.repl :refer (refresh refresh-all)]))

(set! *warn-on-reflection* true)
