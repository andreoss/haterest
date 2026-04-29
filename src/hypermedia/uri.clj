(ns hypermedia.uri
  (:require [clojure.string :as str])
  (:import (java.net URLEncoder)
           (java.nio.charset StandardCharsets)))

(def ^:private variable #"\{([^{}]+)\}")

(defn templated? [template]
  (boolean (re-find variable template)))

(defn encode [value]
  (-> (URLEncoder/encode (str value) StandardCharsets/UTF_8)
      (str/replace "+" "%20")))

(defn expand [template bindings]
  (str/replace template variable
               (fn [[whole v]]
                 (let [k (keyword v)]
                   (if (contains? bindings k)
                     (encode (get bindings k))
                     whole)))))

(defn route-path [template]
  (str/replace template variable (fn [[_ v]] (str ":" v))))

(defn decode [value]
  (java.net.URLDecoder/decode (str value) StandardCharsets/UTF_8))
