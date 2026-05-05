(ns hypermedia.projection-test
  (:require [clojure.string :as string]
            [clojure.test :refer [deftest is]]
            [hypermedia.api :as api]
            [hypermedia.store.memory :as memory]
            [jsonista.core :as json]))

(def config
  {:resources
   {:author {:fields    {:id {:type :long :identity true} :name {:type :string :required true}}
             :relations {:books {:kind :has-many :target :book :via :author-id}}
             :searches  {:by-name {:predicates [:name]}}}
    :book   {:fields      {:id        {:type :long :identity true}
                           :title     {:type :string :required true}
                           :year      {:type :long}
                           :author-id {:type :long}}
             :relations   {:author {:kind :belongs-to :target :author :via :author-id}}
             :projections {:summary {:fields [:title]}
                           :full    {:fields [:title :year] :embed [:author]}}
             :searches    {:by-title {:predicates [:title]}
                           :by-year  {:predicates [:year]}}}}})

(api/defapi demo config)

(def handler
  (api/handler demo
               (memory/store {:author {1 {:id 1 :name "Herbert"} 2 {:id 2 :name "Austen"}}
                              :book   {1 {:id 1 :title "Dune" :year 1965 :author-id 1}
                                       2 {:id 2 :title "Messiah" :year 1969 :author-id 1}
                                       3 {:id 3 :title "Emma" :year 1815 :author-id 2}}})))

(defn- GET [path]
  (let [[uri query] (string/split path #"\?" 2)
        response    (handler {:request-method :get :uri uri :query-string query})]
    (assoc response :body (some-> (:body response) (json/read-value json/keyword-keys-object-mapper)))))

(deftest a-projection-narrows-a-document
  (let [body (:body (GET "/books/1?projection=summary"))]
    (is (= "Dune" (:title body)))
    (is (not (contains? body :year)))
    (is (nil? (:_embedded body)))))

(deftest a-projection-may-embed
  (let [body (:body (GET "/books/1?projection=full"))]
    (is (= 1965 (:year body)))
    (is (= "Herbert" (get-in body [:_embedded :author :name])))))

(deftest a-projection-applies-to-a-collection
  (let [body (:body (GET "/books?projection=summary"))]
    (is (every? #(not (contains? % :year)) (get-in body [:_embedded :books])))
    (is (string/includes? (get-in body [:_links :self :href]) "projection=summary"))))

(deftest a-document-advertises-its-projections
  (is (= "/books/1{?projection}" (get-in (GET "/books/1") [:body :_links :rel:projection :href])))
  (is (true? (get-in (GET "/books/1") [:body :_links :rel:projection :templated])))
  (is (nil? (get-in (GET "/authors/1") [:body :_links :rel:projection]))))

(deftest an-unknown-projection-is-refused
  (is (= 400 (:status (GET "/books/1?projection=nope"))))
  (is (= 400 (:status (GET "/books?projection=nope")))))

(deftest a-collection-links-its-searches
  (is (= "/books/search" (get-in (GET "/books") [:body :_links :search :href])))
  (is (nil? (get-in (GET "/books/1") [:body :_links :search]))))

(deftest the-search-index-lists-what-can-be-asked
  (let [links (get-in (GET "/books/search") [:body :_links])]
    (is (= "/books/search" (get-in links [:self :href])))
    (is (= "/books/search/by-title{?title,page,size,sort}" (get-in links [:rel:by-title :href])))
    (is (true? (get-in links [:rel:by-title :templated])))))

(deftest a-search-answers-a-slice
  (let [body (:body (GET "/books/search/by-title?title=Dune"))]
    (is (= ["Dune"] (map :title (get-in body [:_embedded :books]))))
    (is (= 1 (get-in body [:page :totalElements])))
    (is (= "/books/search/by-title?page=0&size=20" (get-in body [:_links :self :href])))))

(deftest a-search-without-a-predicate-answers-everything
  (is (= 3 (get-in (GET "/books/search/by-title") [:body :page :totalElements]))))

(deftest a-search-refuses-a-predicate-it-cannot-read
  (is (= 400 (:status (GET "/books/search/by-year?year=soon")))))

(deftest a-search-path-does-not-shadow-an-item
  (is (= 200 (:status (GET "/books/1"))))
  (is (= 200 (:status (GET "/books/search")))))

(deftest the-profile-states-projections-and-searches
  (let [response (handler {:request-method :get :uri "/profile/books"
                           :headers {"accept" "application/alps+json"}})
        alps     (-> (:body response) (json/read-value json/keyword-keys-object-mapper) :alps)
        by-id    (into {} (map (juxt :id identity)) (:descriptor alps))]
    (is (= "one of summary, full"
           (get-in (first (:descriptor (get by-id "get-book"))) [:doc :value])))
    (is (= ["title" "page" "size" "sort"]
           (map :name (:descriptor (get by-id "search-by-title")))))))
