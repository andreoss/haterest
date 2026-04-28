(ns hypermedia.store.memory-test
  (:require [clojure.test :refer [deftest is]]
            [hypermedia.schema :as schema]
            [hypermedia.store :as store]
            [hypermedia.store.memory :as memory]))

(def model
  (schema/parse
   {:resources
    {:book {:fields {:id        {:type :long :identity true}
                     :title     {:type :string}
                     :author-id {:type :long}}}}}))

(def book (get-in model [:resources :book]))

(def subject
  (memory/store {:book {1 {:id 1 :title "Dune" :author-id 1}
                        2 {:id 2 :title "Messiah" :author-id 1}
                        3 {:id 3 :title "Emma" :author-id 2}}}))

(deftest fetches-by-identity
  (is (= "Dune" (:title (store/fetch subject book 1))))
  (is (nil? (store/fetch subject book 99))))

(deftest filters-orders-and-slices
  (is (= 3 (count (store/query subject book {}))))
  (is (= ["Dune" "Messiah"] (map :title (store/query subject book {:where {:author-id 1}
                                                                   :order [[:title :asc]]}))))
  (is (= ["Messiah" "Emma" "Dune"] (map :title (store/query subject book {:order [[:title :desc]]}))))
  (is (= ["Emma"] (map :title (store/query subject book {:order [[:title :asc]] :limit 1 :offset 1})))))

(deftest counts-matching-rows
  (is (= 3 (store/total subject book {})))
  (is (= 2 (store/total subject book {:where {:author-id 1}}))))

(deftest refuses-criteria-outside-the-schema
  (is (thrown? clojure.lang.ExceptionInfo (store/query subject book {:where {:sneaky 1}})))
  (is (thrown? clojure.lang.ExceptionInfo (store/total subject book {:where {:sneaky 1}}))))

(deftest probes
  (is (true? (store/probe subject))))

(deftest matches-a-set
  (is (= #{"Dune" "Emma"} (set (map :title (store/query subject book {:where {:id #{1 3}}})))))
  (is (= 2 (store/total subject book {:where {:id #{1 3}}})))
  (is (empty? (store/query subject book {:where {:id #{}}}))))
