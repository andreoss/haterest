(ns hypermedia.schema-test
  (:require [clojure.test :refer [deftest is testing]]
            [hypermedia.schema :as schema]))

(def config
  {:resources
   {:author {:fields    {:id   {:type :uuid :identity true}
                         :name {:type :string :required true}}
             :relations {:books {:kind :has-many :target :book :via :author-id}}}
    :book   {:collection :books
             :fields     {:id        {:type :uuid :identity true}
                          :title     {:type :string :required true}
                          :author-id {:type :uuid}}
             :relations  {:author {:kind :belongs-to :target :author :via :author-id}}}}})

(deftest parses-resources
  (let [model (schema/parse config)]
    (is (= #{:author :book} (set (keys (:resources model)))))
    (is (= [:author :book] (:order model)))))

(deftest derives-defaults
  (let [author (get-in (schema/parse config) [:resources :author])]
    (is (= :author (:name author)))
    (is (= :authors (:collection author)))
    (is (= "/authors" (:path author)))
    (is (= :authors (:table author)))
    (is (= :id (:identity author)))))

(deftest honours-explicit-collection
  (let [book (get-in (schema/parse config) [:resources :book])]
    (is (= :books (:collection book)))
    (is (= "/books" (:path book)))))

(deftest normalises-fields
  (let [fields (get-in (schema/parse config) [:resources :book :fields])]
    (is (= [:id :title :author-id] (get-in (schema/parse config) [:resources :book :field-order])))
    (is (true? (get-in fields [:id :identity?])))
    (is (true? (get-in fields [:title :required?])))
    (is (false? (get-in fields [:author-id :required?])))
    (is (= :author_id (get-in fields [:author-id :column])))))

(deftest normalises-relations
  (let [rel (get-in (schema/parse config) [:resources :book :relations :author])]
    (is (= :author (:name rel)))
    (is (= :belongs-to (:kind rel)))
    (is (= :author (:target rel)))
    (is (= :author-id (:via rel)))
    (is (= "/books/{id}/author" (:path rel)))))

(deftest rejects-malformed-config
  (testing "a resource without identity"
    (is (thrown? clojure.lang.ExceptionInfo
                 (schema/parse {:resources {:a {:fields {:x {:type :string}}}}}))))
  (testing "a relation to an unknown resource"
    (is (thrown? clojure.lang.ExceptionInfo
                 (schema/parse {:resources {:a {:fields    {:id {:type :uuid :identity true}}
                                                :relations {:b {:kind :belongs-to :target :nope :via :id}}}}}))))
  (testing "an unknown field type"
    (is (thrown? clojure.lang.ExceptionInfo
                 (schema/parse {:resources {:a {:fields {:id {:type :wat :identity true}}}}}))))
  (testing "two resources claiming one path"
    (is (thrown? clojure.lang.ExceptionInfo
                 (schema/parse {:resources {:a {:collection :xs :fields {:id {:type :uuid :identity true}}}
                                            :b {:collection :xs :fields {:id {:type :uuid :identity true}}}}})))))

(deftest reports-every-error-at-once
  (let [e (try (schema/parse {:resources {:a {:fields {:x {:type :wat}}}}})
               (catch clojure.lang.ExceptionInfo e e))]
    (is (<= 2 (count (:errors (ex-data e)))))))

(deftest coerces-path-values-by-type
  (is (= 7 (schema/coerce :long "7")))
  (is (= "7" (schema/coerce :string "7")))
  (is (= (java.util.UUID/fromString "0-0-0-0-1")
         (schema/coerce :uuid "0-0-0-0-1")))
  (is (true? (schema/coerce :boolean "true")))
  (is (nil? (schema/coerce :long "seven"))))
