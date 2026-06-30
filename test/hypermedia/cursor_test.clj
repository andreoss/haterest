(ns hypermedia.cursor-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hypermedia.cursor :as cursor]
            [hypermedia.schema :as schema]))

(def model
  (schema/parse
   {:resources {:book {:fields {:id    {:type :long :identity true}
                                :title {:type :string}
                                :year  {:type :long}}}}}))

(def book (get-in model [:resources :book]))

(deftest a-cursor-carries-the-place-it-left-off
  (let [text (cursor/of book [[:title :asc]] {:id 7 :title "Dune"} 3)]
    (is (string? text))
    (is (= {:after ["Dune" 7] :number 3} (cursor/read-from book [[:title :asc]] text)))))

(deftest a-cursor-with-no-sort-follows-the-identity
  (let [text (cursor/of book [] {:id 7 :title "Dune"} 1)]
    (is (= {:after [7] :number 1} (cursor/read-from book [] text)))))

(deftest a-cursor-survives-a-value-with-punctuation-in-it
  (let [text (cursor/of book [[:title :asc]] {:id 7 :title "a|b c%d"} 2)]
    (is (= {:after ["a|b c%d" 7] :number 2} (cursor/read-from book [[:title :asc]] text)))))

(deftest a-cursor-is-opaque
  (let [text (cursor/of book [[:title :asc]] {:id 7 :title "Dune"} 3)]
    (is (not (str/includes? text "Dune")))
    (is (re-matches #"[A-Za-z0-9_-]+" text))))

(deftest a-cursor-for-another-ordering-is-refused
  (let [text (cursor/of book [[:title :asc]] {:id 7 :title "Dune"} 3)]
    (is (:mismatched (cursor/read-from book [[:year :asc]] text)))
    (is (:mismatched (cursor/read-from book [[:title :desc]] text)))))

(deftest nonsense-is-refused-rather-than-obeyed
  (doseq [text ["" "not-base64!!" "YWJj"]]
    (is (:mismatched (cursor/read-from book [[:title :asc]] text)) (str "for " text))))

(deftest a-row-with-nothing-to-sort-by-yields-no-cursor
  (is (nil? (cursor/of book [[:title :asc]] {:id 7 :title nil} 1))))
