(ns hypermedia.slice-test
  (:require [clojure.test :refer [deftest is]]
            [hypermedia.api :as api]
            [hypermedia.store.memory :as memory]
            [jsonista.core :as json]))

(def config
  {:resources
   {:author {:fields    {:id {:type :long :identity true} :name {:type :string}}
             :relations {:books {:kind :has-many :target :book :via :author-id}}}
    :book   {:fields    {:id        {:type :long :identity true}
                         :title     {:type :string}
                         :author-id {:type :long}}
             :relations {:author {:kind :belongs-to :target :author :via :author-id}}}}})

(api/defapi demo config)

(def titles ["Dune" "Emma" "Ficciones" "Gormenghast" "Hyperion"])

(def handler
  (api/handler demo
               (memory/store {:author {1 {:id 1 :name "Herbert"}}
                              :book   (into {} (map-indexed (fn [i t]
                                                              [(inc i) {:id (inc i) :title t :author-id 1}])
                                                            titles))})))

(defn- GET [path]
  (let [[uri query] (clojure.string/split path #"\?" 2)
        response    (handler {:request-method :get :uri uri :query-string query})]
    (assoc response :body (some-> (:body response) (json/read-value json/keyword-keys-object-mapper)))))

(defn- titles-of [body] (mapv :title (get-in body [:_embedded :books])))

(deftest describes-the-whole-collection
  (let [body (:body (GET "/books"))]
    (is (= {:size 20 :totalElements 5 :totalPages 1 :number 0} (:page body)))
    (is (= 5 (count (titles-of body))))))

(deftest slices-and-sorts
  (let [body (:body (GET "/books?page=1&size=2&sort=title,asc"))]
    (is (= ["Ficciones" "Gormenghast"] (titles-of body)))
    (is (= {:size 2 :totalElements 5 :totalPages 3 :number 1} (:page body)))))

(deftest links-the-neighbouring-slices
  (let [links (get-in (GET "/books?page=1&size=2&sort=title,asc") [:body :_links])]
    (is (= "/books?page=1&size=2&sort=title%2Casc" (get-in links [:self :href])))
    (is (= "/books?page=0&size=2&sort=title%2Casc" (get-in links [:prev :href])))
    (is (= "/books?page=2&size=2&sort=title%2Casc" (get-in links [:next :href])))
    (is (= "/books?page=2&size=2&sort=title%2Casc" (get-in links [:last :href])))))

(deftest a-client-walks-the-collection-by-following-next
  (loop [href "/books?size=2&sort=title,asc" seen [] guard 0]
    (let [body (:body (GET href))
          seen (into seen (titles-of body))]
      (if-let [next (get-in body [:_links :next :href])]
        (if (< guard 10) (recur next seen (inc guard)) (is false "the walk did not end"))
        (is (= titles seen))))))

(deftest slices-an-association
  (let [body (:body (GET "/authors/1/books?size=2&sort=title,desc"))]
    (is (= ["Hyperion" "Gormenghast"] (titles-of body)))
    (is (= 5 (get-in body [:page :totalElements])))
    (is (= "/authors/1/books?page=1&size=2&sort=title%2Cdesc" (get-in body [:_links :next :href])))))

(deftest refuses-a-slice-it-cannot-read
  (is (= 400 (:status (GET "/books?page=-1"))))
  (is (= 400 (:status (GET "/books?sort=sneaky"))))
  (is (= 400 (:status (GET "/books?size=none")))))
