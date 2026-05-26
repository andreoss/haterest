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

(defn compiled [template]
  (let [matcher (re-matcher variable template)]
    (loop [parts [] at 0]
      (if (.find matcher)
        (recur (-> parts
                   (conj (subs template at (.start matcher)))
                   (conj (keyword (.group matcher 1))))
               (.end matcher))
        (conj parts (subs template at))))))

(defn render [parts bindings]
  (let [out (StringBuilder.)]
    (doseq [part parts]
      (.append out (if (keyword? part) (encode (get bindings part)) ^String part)))
    (.toString out)))
