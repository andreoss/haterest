(ns hypermedia.negotiate
  (:require [clojure.string :as str]))

(defn- range-of [text]
  (let [[media & parameters] (map str/trim (str/split text #";"))
        quality (some (fn [p] (when (str/starts-with? p "q=")
                                (try (Double/parseDouble (subs p 2)) (catch Exception _ nil))))
                      parameters)]
    {:media (str/lower-case media) :quality (or quality 1.0)}))

(defn ranges [header]
  (if (str/blank? header)
    [{:media "*/*" :quality 1.0}]
    (sort-by :quality > (map range-of (str/split header #",")))))

(defn- fits? [media offer]
  (let [[type subtype] (str/split offer #"/")]
    (or (= media "*/*")
        (= media offer)
        (= media (str type "/*")))))

(defn choose [header offers]
  (some (fn [{:keys [media quality]}]
          (when (pos? quality) (first (filter #(fits? media %) offers))))
        (ranges header)))
