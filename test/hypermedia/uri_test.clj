(ns hypermedia.uri-test
  (:require [clojure.test :refer [deftest is]]
            [hypermedia.uri :as uri]))

(deftest expands-templates
  (is (= "/books/7" (uri/expand "/books/{id}" {:id 7})))
  (is (= "/books/7/author" (uri/expand "/books/{id}/author" {:id 7}))))

(deftest leaves-unbound-variables
  (is (= "/books/{id}" (uri/expand "/books/{id}" {}))))

(deftest detects-templates
  (is (true? (uri/templated? "/books/{id}")))
  (is (false? (uri/templated? "/books"))))

(deftest converts-to-route-syntax
  (is (= "/books/:id" (uri/route-path "/books/{id}")))
  (is (= "/books" (uri/route-path "/books"))))

(deftest encodes-segments
  (is (= "/books/a%20b" (uri/expand "/books/{id}" {:id "a b"}))))

(deftest compiles-a-template-into-parts
  (is (= ["/books/" :id ""] (uri/compiled "/books/{id}")))
  (is (= ["/books/" :id "/author"] (uri/compiled "/books/{id}/author")))
  (is (= ["/books"] (uri/compiled "/books")))
  (is (= ["/a/" :x "/b/" :y ""] (uri/compiled "/a/{x}/b/{y}"))))

(deftest renders-compiled-parts
  (is (= "/books/7" (uri/render (uri/compiled "/books/{id}") {:id 7})))
  (is (= "/books/7/author" (uri/render (uri/compiled "/books/{id}/author") {:id 7})))
  (is (= "/books" (uri/render (uri/compiled "/books") {})))
  (is (= "/books/a%20b" (uri/render (uri/compiled "/books/{id}") {:id "a b"}))))

(deftest a-rendered-template-matches-an-expanded-one
  (doseq [template ["/books" "/books/{id}" "/books/{id}/author" "/a/{x}/b/{y}"]]
    (is (= (uri/expand template {:id 7 :x "p q" :y 9})
           (uri/render (uri/compiled template) {:id 7 :x "p q" :y 9}))
        template)))
