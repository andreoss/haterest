(ns hypermedia.rel-test
  (:require [clojure.test :refer [deftest is]]
            [hypermedia.rel :as rel]))

(deftest leaves-registered-relations-alone
  (is (= :self (rel/curied "rel" :self)))
  (is (= :next (rel/curied "rel" :next)))
  (is (= :profile (rel/curied "rel" :profile))))

(deftest prefixes-relations-the-platform-invents
  (is (= (keyword "rel:books") (rel/curied "rel" :books)))
  (is (= (keyword "x:books") (rel/curied "x" :books))))

(deftest a-schema-may-decline-a-curie
  (is (= :books (rel/curied nil :books)))
  (is (= :books (rel/curied false :books))))

(deftest declares-the-curie-once
  (is (= [{:name "rel" :href "/profile/{rel}" :templated true}]
         (rel/curies "rel")))
  (is (nil? (rel/curies nil))))
