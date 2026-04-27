(ns hypermedia.negotiate-test
  (:require [clojure.test :refer [deftest is]]
            [hypermedia.negotiate :as negotiate]))

(def offers ["application/hal+json" "application/json"])

(deftest an-absent-header-takes-the-first-offer
  (is (= "application/hal+json" (negotiate/choose nil offers)))
  (is (= "application/hal+json" (negotiate/choose "" offers))))

(deftest matches-an-exact-type
  (is (= "application/json" (negotiate/choose "application/json" offers))))

(deftest matches-a-wildcard
  (is (= "application/hal+json" (negotiate/choose "*/*" offers)))
  (is (= "application/hal+json" (negotiate/choose "application/*" offers))))

(deftest honours-quality
  (is (= "application/json" (negotiate/choose "application/hal+json;q=0.1, application/json;q=0.9" offers)))
  (is (nil? (negotiate/choose "application/hal+json;q=0" ["application/hal+json"]))))

(deftest reports-nothing-when-it-cannot-serve
  (is (nil? (negotiate/choose "text/csv" offers)))
  (is (nil? (negotiate/choose "text/*" offers))))
