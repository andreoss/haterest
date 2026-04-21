(ns hypermedia.page-test
  (:require [clojure.test :refer [deftest is]]
            [hypermedia.page :as page]
            [hypermedia.schema :as schema]))

(def model
  (schema/parse
   {:resources
    {:book {:fields {:id    {:type :long :identity true}
                     :title {:type :string}
                     :year  {:type :long}}}}}))

(def book (get-in model [:resources :book]))

(deftest defaults-to-the-first-page
  (let [pageable (page/parse book {})]
    (is (= 0 (:number pageable)))
    (is (= page/default-size (:size pageable)))
    (is (empty? (:sort pageable)))
    (is (empty? (:errors pageable)))))

(deftest reads-page-size-and-sort
  (let [pageable (page/parse book {"page" "2" "size" "5" "sort" "title,desc"})]
    (is (= 2 (:number pageable)))
    (is (= 5 (:size pageable)))
    (is (= [[:title :desc]] (:sort pageable)))))

(deftest reads-several-sort-clauses
  (is (= [[:title :asc] [:year :desc]]
         (:sort (page/parse book {"sort" ["title" "year,desc"]})))))

(deftest refuses-what-it-cannot-read
  (is (seq (:errors (page/parse book {"page" "-1"}))))
  (is (seq (:errors (page/parse book {"size" "0"}))))
  (is (seq (:errors (page/parse book {"size" "soon"}))))
  (is (seq (:errors (page/parse book {"sort" "sneaky,asc"}))))
  (is (seq (:errors (page/parse book {"sort" "title,sideways"})))))

(deftest caps-the-size
  (is (= page/maximum-size (:size (page/parse book {"size" "100000"})))))

(deftest turns-into-store-criteria
  (is (= {:where {:year 1965} :order [[:title :asc]] :limit 5 :offset 10}
         (page/criteria (page/parse book {"page" "2" "size" "5" "sort" "title"}) {:year 1965}))))

(deftest builds-hrefs-that-keep-the-slice
  (let [pageable (page/parse book {"page" "1" "size" "5" "sort" "title,desc"})]
    (is (= "/books?page=0&size=5&sort=title%2Cdesc" (page/href "/books" pageable 0)))))

(deftest links-the-neighbours-of-a-slice
  (let [pageable (page/parse book {"page" "1" "size" "5"})
        links    (page/links "/books" pageable 21)]
    (is (= "/books?page=1&size=5" (:href (:self links))))
    (is (= "/books?page=0&size=5" (:href (:first links))))
    (is (= "/books?page=0&size=5" (:href (:prev links))))
    (is (= "/books?page=2&size=5" (:href (:next links))))
    (is (= "/books?page=4&size=5" (:href (:last links))))))

(deftest omits-neighbours-that-do-not-exist
  (let [links (page/links "/books" (page/parse book {"size" "5"}) 3)]
    (is (nil? (:prev links)))
    (is (nil? (:next links)))
    (is (= "/books?page=0&size=5" (:href (:last links))))))

(deftest describes-the-slice
  (is (= {:size 5 :totalElements 21 :totalPages 5 :number 1}
         (page/descriptor (page/parse book {"page" "1" "size" "5"}) 21))))

(deftest an-empty-collection-is-one-page
  (is (= 1 (:totalPages (page/descriptor (page/parse book {}) 0)))))
