(ns hypermedia.etag
  (:require [clojure.string :as str]
            [jsonista.core :as json])
  (:import (java.nio.charset StandardCharsets)
           (java.security MessageDigest)
           (java.util Base64)))

(def ^:private mapper (json/object-mapper {:encode-key-fn name}))

(defn- canonical [row]
  (json/write-value-as-string
   (into (sorted-map) (map (fn [[k v]] [k (some-> v str)])) row)
   mapper))

(defn of [row]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256")
                        (.getBytes (canonical row) StandardCharsets/UTF_8))]
    (str "\"" (.encodeToString (.withoutPadding (Base64/getUrlEncoder)) digest) "\"")))

(defn matches? [header tag]
  (if (str/blank? header)
    false
    (boolean (some #(or (= % "*") (= % tag))
                   (map str/trim (str/split header #","))))))
