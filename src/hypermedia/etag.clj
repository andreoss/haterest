(ns hypermedia.etag
  (:require [clojure.string :as str]))

(defn of [version]
  (str "\"" (or version 0) "\""))

(defn wildcard? [header]
  (= "*" (str/trim (str header))))

(defn- quoted-version [tag]
  (let [text (str/trim tag)]
    (when (and (str/starts-with? text "\"") (str/ends-with? text "\"") (< 2 (count text)))
      (try (Long/parseLong (subs text 1 (dec (count text))))
           (catch Exception _ nil)))))

(defn versions-in [header]
  (if (str/blank? header)
    []
    (vec (keep quoted-version (str/split header #",")))))

(defn matches? [header version]
  (cond
    (str/blank? header) false
    (wildcard? header)  true
    :else               (contains? (set (versions-in header)) version)))
