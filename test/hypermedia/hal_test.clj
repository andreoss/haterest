(ns hypermedia.hal-test
  (:require [clojure.test :refer [deftest is]]
            [hypermedia.hal :as hal]))

(deftest builds-links
  (is (= {:href "/books"} (hal/link "/books")))
  (is (= {:href "/books/{id}" :templated true} (hal/link "/books/{id}"))))

(deftest carries-link-attributes
  (is (= {:href "/books" :title "Books" :type "application/hal+json"}
         (hal/link "/books" {:title "Books" :type "application/hal+json"}))))

(deftest builds-a-document
  (is (= {:title "Dune" :_links {:self {:href "/books/1"}}}
         (hal/document {:title "Dune"} {:self (hal/link "/books/1")}))))

(deftest omits-empty-sections
  (is (= {:title "Dune"} (hal/document {:title "Dune"} {}))))

(deftest embeds-resources
  (let [doc (hal/document {} {:self (hal/link "/books")}
                          {:books [(hal/document {:title "Dune"} {})]})]
    (is (= [{:title "Dune"}] (get-in doc [:_embedded :books])))))

(deftest reports-its-media-type
  (is (= "application/hal+json" hal/media-type)))
