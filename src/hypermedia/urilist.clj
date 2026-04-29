(ns hypermedia.urilist
  (:require [clojure.string :as str]))

(def media-type "text/uri-list")

(defn parse [text]
  (->> (str/split-lines (or text ""))
       (map str/trim)
       (remove str/blank?)
       (remove #(str/starts-with? % "#"))
       vec))
