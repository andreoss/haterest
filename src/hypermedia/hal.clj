(ns hypermedia.hal
  (:require [hypermedia.uri :as uri]))

(def media-type "application/hal+json")

(defn href
  ([target] {:href target})
  ([target attributes] (if attributes (into {:href target} attributes) {:href target})))

(defn link
  ([target] (link target nil))
  ([target attributes]
   (cond-> (href target attributes)
     (uri/templated? target) (assoc :templated true))))

(defn document
  ([properties links] (document properties links nil))
  ([properties links embedded]
   (cond-> (into {} properties)
     (seq links)    (assoc :_links (into {} links))
     (seq embedded) (assoc :_embedded (into {} embedded)))))
