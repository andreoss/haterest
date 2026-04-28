(ns hypermedia.route
  (:require [hypermedia.uri :as uri]))

(defn member-variable [relation]
  (keyword (str (name (:target relation)) "-key")))

(defn member-template [relation]
  (str (:path relation) "/{" (name (member-variable relation)) "}"))

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
        (mapcat
         (fn [[k relation]]
           (cond-> [[(uri/route-path (:path relation))
                     {:name                (keyword "hypermedia.route"
                                                    (str (name (:name resource)) ".association." (name k)))
                      :hypermedia/resource (:name resource)
                      :hypermedia/op       :association
                      :hypermedia/relation relation
                      :hypermedia/template (:path relation)}]]
             (= :has-many (:kind relation))
             (conj [(uri/route-path (member-template relation))
                    {:name                (keyword "hypermedia.route"
                                                   (str (name (:name resource)) ".member." (name k)))
                     :hypermedia/resource (:name resource)
                     :hypermedia/op       :association-member
                     :hypermedia/relation relation
                     :hypermedia/template (member-template relation)}])))
         (:relations resource))))

(defn routes [model]
  (into [["/" {:name                :hypermedia.route/root
               :hypermedia/op       :root
               :hypermedia/template "/"}]]
        (mapcat #(resource-routes (get-in model [:resources %])))
        (:order model)))
