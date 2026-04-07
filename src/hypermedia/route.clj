(ns hypermedia.route
  (:require [hypermedia.uri :as uri]))

(defn- item-template [resource]
  (str (:path resource) "/{" (name (:identity resource)) "}"))

(defn- resource-routes [resource]
  (into [[(uri/route-path (:path resource))
          {:name                (keyword "hypermedia.route" (str (name (:name resource)) ".collection"))
           :hypermedia/resource (:name resource)
           :hypermedia/op       :collection
           :hypermedia/template (:path resource)}]
         [(uri/route-path (item-template resource))
          {:name                (keyword "hypermedia.route" (str (name (:name resource)) ".item"))
           :hypermedia/resource (:name resource)
           :hypermedia/op       :item
           :hypermedia/template (item-template resource)}]]
        (for [[k relation] (:relations resource)]
          [(uri/route-path (:path relation))
           {:name                (keyword "hypermedia.route"
                                          (str (name (:name resource)) ".association." (name k)))
            :hypermedia/resource (:name resource)
            :hypermedia/op       :association
            :hypermedia/relation relation
            :hypermedia/template (:path relation)}])))

(defn routes [model]
  (into [["/" {:name                :hypermedia.route/root
               :hypermedia/op       :root
               :hypermedia/template "/"}]]
        (mapcat #(resource-routes (get-in model [:resources %])))
        (:order model)))
