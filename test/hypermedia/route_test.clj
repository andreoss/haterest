(ns hypermedia.route-test
  (:require [clojure.test :refer [deftest is]]
            [hypermedia.schema :as schema]
            [hypermedia.route :as route]))

(def model
  (schema/parse
   {:resources
    {:author {:fields    {:id {:type :uuid :identity true} :name {:type :string}}
              :relations {:books {:kind :has-many :target :book :via :author-id}}}
     :book   {:fields    {:id {:type :uuid :identity true} :title {:type :string}
                          :author-id {:type :uuid}}
              :relations {:author {:kind :belongs-to :target :author :via :author-id}}}}}))

(defn- paths [routes] (mapv first routes))

(deftest roots-the-api
  (is (= "/" (first (first (route/routes model))))))

(deftest emits-collection-item-and-association-routes
  (let [p (set (paths (route/routes model)))]
    (is (contains? p "/authors"))
    (is (contains? p "/authors/:id"))
    (is (contains? p "/authors/:id/books"))
    (is (contains? p "/books"))
    (is (contains? p "/books/:id"))
    (is (contains? p "/books/:id/author"))))

(deftest tags-each-route-with-its-resource-and-operation
  (let [by-path (into {} (route/routes model))]
    (is (= :book (get-in by-path ["/books" :hypermedia/resource])))
    (is (= :collection (get-in by-path ["/books" :hypermedia/op])))
    (is (= :item (get-in by-path ["/books/:id" :hypermedia/op])))
    (is (= :association (get-in by-path ["/books/:id/author" :hypermedia/op])))
    (is (= :author (get-in by-path ["/books/:id/author" :hypermedia/relation :target])))))

(deftest names-every-route
  (is (every? :name (map second (route/routes model)))))

(deftest addresses-a-member-of-a-has-many-relation
  (let [by-path (into {} (route/routes model))]
    (is (contains? by-path "/authors/:id/books/:book-key"))
    (is (= :association-member (get-in by-path ["/authors/:id/books/:book-key" :hypermedia/op])))
    (is (not (contains? by-path "/books/:id/author/:author-key")))))
