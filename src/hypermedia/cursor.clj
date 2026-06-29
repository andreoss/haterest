(ns hypermedia.cursor
  (:require [clojure.string :as str]
            [hypermedia.schema :as schema]
            [hypermedia.sql :as sql]
            [hypermedia.uri :as uri])
  (:import (java.nio.charset StandardCharsets)
           (java.util Base64)))

(def ^:private encoder (.withoutPadding (Base64/getUrlEncoder)))

(def ^:private decoder (Base64/getUrlDecoder))

(defn- shape [resource order]
  (str/join "," (map (fn [[field direction]] (str (name field) ":" (name direction)))
                     (sql/total-order resource order))))

(defn of [resource order row number]
  (let [fields (mapv first (sql/total-order resource order))
        values (mapv #(get row %) fields)]
    (when (every? some? values)
      (let [plain (str/join "|" (concat [(uri/encode (shape resource order)) (str number)]
                                        (map #(uri/encode (str %)) values)))]
        (.encodeToString encoder (.getBytes ^String plain StandardCharsets/UTF_8))))))

(defn read-from [resource order text]
  (try
    (let [plain  (String. (.decode decoder ^String text) StandardCharsets/UTF_8)
          parts  (str/split plain #"\|")
          given  (uri/decode (first parts))
          number (second parts)
          values (map uri/decode (drop 2 parts))
          fields (mapv first (sql/total-order resource order))]
      (cond
        (not= given (shape resource order))  {:mismatched true}
        (not= (count values) (count fields)) {:mismatched true}
        :else
        (let [read (mapv (fn [field value]
                           (schema/coerce (get-in resource [:fields field :type]) value))
                         fields values)]
          (if (some nil? read)
            {:mismatched true}
            {:after read :number (parse-long number)}))))
    (catch Exception _ {:mismatched true})))
