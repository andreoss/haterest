(ns hypermedia.etag-test
  (:require [clojure.test :refer [deftest is]]
            [hypermedia.etag :as etag]))

(deftest a-tag-names-the-version-it-was-read-at
  (is (= "\"0\"" (etag/of 0)))
  (is (= "\"7\"" (etag/of 7)))
  (is (= "\"0\"" (etag/of nil))))

(deftest a-tag-changes-with-every-write
  (is (not= (etag/of 7) (etag/of 8))))

(deftest versions-are-read-back-out-of-a-header
  (is (= [7] (etag/versions-in (etag/of 7))))
  (is (= [3 7] (etag/versions-in "\"3\", \"7\"")))
  (is (= [] (etag/versions-in "W/\"7\"")))
  (is (= [] (etag/versions-in "nonsense")))
  (is (= [] (etag/versions-in nil))))

(deftest a-wildcard-asks-for-whatever-is-current
  (is (true? (etag/wildcard? "*")))
  (is (true? (etag/wildcard? " * ")))
  (is (false? (etag/wildcard? "\"7\""))))

(deftest any-tag-in-the-header-satisfies-the-precondition
  (is (true? (etag/matches? (etag/of 7) 7)))
  (is (true? (etag/matches? "\"3\", \"7\"" 7)))
  (is (true? (etag/matches? "*" 7)))
  (is (false? (etag/matches? (etag/of 6) 7)))
  (is (false? (etag/matches? nil 7)))
  (is (false? (etag/matches? "" 7))))
