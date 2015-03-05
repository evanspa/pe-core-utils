(ns pe-core-utils.core
  "A set of functions, not particular to any one domain."
  (:require [clojure.walk :refer [postwalk]]))

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
(defn throwable->str
  "Returns throwable as a string."
  [throwable]
  (let [sw (java.io.StringWriter.)
        pw (java.io.PrintWriter. sw)]
    (.toString sw)))

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
  [^String keyword]
  (.substring (.toString keyword) 1))

(defn ->jsonkeys
  "Replaces all the keyword-based keys in m with JSON string keys."
  [m]
  (replace-keys m (keys m) keyword->jsonkey))
