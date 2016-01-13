(ns pe-core-utils.core
  "A set of functions, not particular to any one domain."
  (:require [clojure.walk :refer [postwalk]]
            [clj-time.core :as t]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Date/Time Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def http-date-format-pattern
  "Internet date (RFC 7231) format pattern."
  "EEE, dd MMM yyyy HH:mm:ss z")

(defn rfc7231str->instant
  "Returns an instance given an internet date (RFC 7231) string."
  [s]
  (.parse (java.text.SimpleDateFormat. http-date-format-pattern) s))

(defn instant->rfc7231str
  "Returns an internet date (RFC 7231) string from the given instant."
  [inst]
  (let [sdf (java.text.SimpleDateFormat. http-date-format-pattern)]
    (.setTimeZone sdf (java.util.TimeZone/getTimeZone "GMT"))
    (.format sdf inst)))

(defn rfc7231str-dates->instants
  "Traverses the map m and replaces every internet date (RFC 7231) string with
  an instant instance."
  [m]
  (postwalk (fn [key-or-val]
              (if (= java.lang.String (class key-or-val))
                (try
                  (rfc7231str->instant key-or-val)
                  (catch java.text.ParseException e key-or-val))
                key-or-val))
            m))

(defn instants->rfc7231str-dates
  "Traverses the map m and replaces every instant with an internet date (RFC
  7231) string."
  [m]
  (postwalk (fn [key-or-val]
              (if (= java.util.Date (class key-or-val))
                (instant->rfc7231str key-or-val)
                key-or-val))
            m))

(defn d->instant
  "Converts the date (java.util.Date) to an instant."
  [d]
  (.toDate d))

(defn now->instant
  "Returns now as an instant."
  []
  (d->instant (t/now)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Validation Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn add-condition
  "If pred-fn evaluates to true, returns the bitwise or of mask, mask-add and
  any-issues-bit.  Otherwise returns mask."
  [mask pred-fn mask-add any-issues-bit]
  (if (pred-fn)
    (bit-or mask mask-add any-issues-bit)
    mask))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn remove-matches
  "Produces a new collection from coll where f applied to all pairs of elements
  of coll that evaluate to true are not included in the new collection."
  [coll f]
  (let [num-items (count coll)]
    (if (or (= 1 num-items)
            (= 0 num-items))
      coll
      (loop [i 1
             result-coll [(first coll)]]
        (if (= i num-items)
          result-coll
          (recur (inc i)
                 (let [num-result-items (count result-coll)
                       result-item (let [outer-item (nth coll i)]
                                     (loop [j 0
                                            item nil]
                                       (if (>= j num-result-items)
                                         item
                                         (let [inner-item (nth result-coll j)]
                                           (if (f inner-item outer-item)
                                             (recur i nil)
                                             (recur (inc j) outer-item))))))]
                   (if (not (nil? result-item))
                     (conj result-coll result-item)
                     result-coll))))))))

(defn throwable->str
  "Returns throwable as a string."
  [throwable]
  (let [sw (java.io.StringWriter.)
        pw (java.io.PrintWriter. sw)]
    (.toString sw)))

(defn remove-nils
  "Removes nil entries from m."
  [m]
  (into {}
        (filter (comp not nil? val) m)))

(defn transform-map-val
  "Replaces the value at key in m with the result of applying transform-fn to
  the value at key."
  [m key transform-fn]
  (let [val (key m)]
    (if val
      (assoc m key (transform-fn val))
      m)))

(defn trim-keys
  "Returns m with each key in keys removed."
  [m keys]
  (reduce (fn [new-map key] (dissoc new-map key))
          m
          keys))

(defn replace-keys
  "Returns m with each key transformed by key-transformer-fn."
  [m keys key-transformer-fn]
  (reduce (fn [new-map key]
            (if-let [v (get m key)]
              (assoc new-map (key-transformer-fn key) v)
              m))
          {}
          keys))

(defn keyword->jsonkey
  "Returns keyword as a JSON string (basically just removes the beginning
  colon."
  [keyword]
  (.substring (.toString keyword) 1))

(defn ->jsonkeys
  "Replaces all the keyword-based keys in m with JSON string keys."
  [m]
  (replace-keys m (keys m) keyword->jsonkey))
