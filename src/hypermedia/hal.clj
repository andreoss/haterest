(ns hypermedia.hal
  (:require [hypermedia.uri :as uri]))

(def media-type "application/hal+json")

(defn link
  ([href] (link href nil))
  ([href attributes]
   (cond-> (into {:href href} attributes)
     (uri/templated? href) (assoc :templated true))))

(defn document
  ([properties links] (document properties links nil))
  ([properties links embedded]
   (cond-> (into {} properties)
     (seq links)    (assoc :_links (into {} links))
     (seq embedded) (assoc :_embedded (into {} embedded)))))
