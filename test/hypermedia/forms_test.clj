(ns hypermedia.forms-test
  (:require [clojure.test :refer [deftest is]]
            [hypermedia.forms :as forms]
            [hypermedia.schema :as schema]))

(def model
  (schema/parse
   {:resources
    {:author {:fields {:id {:type :long :identity true} :name {:type :string}}}
     :book   {:fields    {:id        {:type :long :identity true}
                          :title     {:type :string :required true}
                          :year      {:type :long}
                          :read      {:type :boolean}
                          :author-id {:type :long}}
              :relations {:author {:kind :belongs-to :target :author :via :author-id}}}}}))

(def book (get-in model [:resources :book]))

(deftest properties-follow-the-schema
  (is (= [{:name "title" :type "text" :required true}
          {:name "year" :type "number" :required false}
          {:name "read" :type "checkbox" :required false}
          {:name "author" :type "url" :required false}]
         (forms/properties book))))

(deftest a-collection-offers-creation
  (let [template (:default (forms/templates book :collection))]
    (is (= "POST" (:method template)))
    (is (= "/books" (:target template)))))

(deftest an-item-offers-replacement-amendment-and-removal
  (let [templates (forms/templates book :item)]
    (is (= "PUT" (get-in templates [:default :method])))
    (is (= "PATCH" (get-in templates [:patch :method])))
    (is (= "DELETE" (get-in templates [:delete :method])))
    (is (empty? (get-in templates [:delete :properties])))))

(deftest nothing-else-carries-a-template
  (is (nil? (forms/templates book :root))))
