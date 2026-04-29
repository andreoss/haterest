(ns hypermedia.urilist-test
  (:require [clojure.test :refer [deftest is]]
            [hypermedia.urilist :as urilist]))

(deftest reads-one-uri-per-line
  (is (= ["/books/1" "/books/2"] (urilist/parse "/books/1\n/books/2\n"))))

(deftest ignores-blank-lines-and-comments
  (is (= ["/books/1"] (urilist/parse "# a comment\n\n  /books/1  \n"))))

(deftest an-empty-body-is-an-empty-list
  (is (= [] (urilist/parse nil)))
  (is (= [] (urilist/parse ""))))
