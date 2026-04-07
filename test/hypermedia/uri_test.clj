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
