(ns hypermedia.etag-test
  (:require [clojure.test :refer [deftest is]]
            [hypermedia.etag :as etag]))

(deftest the-same-row-yields-the-same-tag
  (is (= (etag/of {:id 1 :title "Dune"}) (etag/of {:title "Dune" :id 1}))))

(deftest a-changed-row-yields-another-tag
  (is (not= (etag/of {:id 1 :title "Dune"}) (etag/of {:id 1 :title "Emma"}))))

(deftest a-tag-is-quoted
  (is (re-matches #"\"[A-Za-z0-9_-]+\"" (etag/of {:id 1}))))

(deftest a-header-is-matched
  (let [tag (etag/of {:id 1})]
    (is (true? (etag/matches? tag tag)))
    (is (true? (etag/matches? "*" tag)))
    (is (true? (etag/matches? (str "\"other\", " tag) tag)))
    (is (false? (etag/matches? "\"other\"" tag)))
    (is (false? (etag/matches? nil tag)))
    (is (false? (etag/matches? "" tag)))))

(deftest bookkeeping-does-not-reach-the-tag
  (is (= (etag/of {:id 1 :title "Dune"})
         (etag/of {:id 1 :title "Dune" :hypermedia/version 7})))
  (is (not= (etag/of {:id 1 :title "Dune"})
            (etag/of {:id 1 :title "Emma" :hypermedia/version 7}))))
