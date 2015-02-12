(ns pe-core-utils.core
  (:require [clojure.walk :refer [postwalk]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Date/Time Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private http-date-format-pattern "EEE, dd MMM yyyy HH:mm:ss z")

(defn rfc7231str->instant [s]
  (.parse (java.text.SimpleDateFormat. http-date-format-pattern) s))

(defn instant->rfc7231str [inst]
  (let [sdf (java.text.SimpleDateFormat. http-date-format-pattern)]
    (.setTimeZone sdf (java.util.TimeZone/getTimeZone "GMT"))
    (.format sdf inst)))

(defn rfc7231str-dates->instants
  [m]
  (postwalk (fn [key-or-val]
              (if (= java.lang.String (class key-or-val))
                (try
                  (rfc7231str->instant key-or-val)
                  (catch java.text.ParseException e key-or-val))
                key-or-val))
            m))

(defn instants->rfc7231str-dates
  [m]
  (postwalk (fn [key-or-val]
              (if (= java.util.Date (class key-or-val))
                (instant->rfc7231str key-or-val)
                key-or-val))
            m))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Validation Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn add-condition [mask pred mask-add any-issues-bit]
  (if (pred)
    (bit-or mask mask-add any-issues-bit)
    mask))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn throwable->str
  [throwable]
  (let [sw (java.io.StringWriter.)
        pw (java.io.PrintWriter. sw)]
    (.printStackTrace throwable pw)
    (.toString sw)))

(defn transform-map-val
  [m key transform-fn]
  (let [val (key m)]
    (if val
      (assoc m key (transform-fn val))
      m)))

(defn trim-keys
  [keys m]
  (reduce (fn [new-map key] (dissoc new-map key))
          m
          keys))

(defn replace-keys
  [keys m key-transformer-fn]
  (reduce (fn [new-map key]
            (if-let [v (get m key)]
              (assoc new-map (key-transformer-fn key) v)
              m))
          {}
          keys))

(defn keyword->jsonkey
  [keyword]
  (.substring (.toString keyword) 1))

(defn ->jsonkeys
  "(i don't think I need this anymore)"
  [m]
  (replace-keys (keys m) m keyword->jsonkey))
