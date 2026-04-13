(ns hypermedia.sql-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hypermedia.schema :as schema]
            [hypermedia.sql :as sql]))

(def model
  (schema/parse
   {:resources
    {:author {:fields    {:id {:type :long :identity true} :name {:type :string :required true}}
              :relations {:books {:kind :has-many :target :book :via :author-id}}}
     :book   {:fields    {:id        {:type :long :identity true}
                          :title     {:type :string :required true}
                          :author-id {:type :long}}
              :relations {:author {:kind :belongs-to :target :author :via :author-id}}}}}))

(deftest detects-the-dialect-from-a-url
  (is (= :h2 (sql/dialect "jdbc:h2:mem:x")))
  (is (= :sqlite (sql/dialect "jdbc:sqlite::memory:")))
  (is (= :postgres (sql/dialect "jdbc:postgresql://host/db")))
  (is (= :ansi (sql/dialect "jdbc:unheard-of:x"))))

(deftest maps-types-per-dialect
  (is (= "BIGINT" (sql/column-type :h2 :long)))
  (is (= "INTEGER" (sql/column-type :sqlite :long)))
  (is (= "UUID" (sql/column-type :h2 :uuid)))
  (is (= "TEXT" (sql/column-type :sqlite :uuid))))

(deftest derives-ddl
  (let [statements (sql/ddl model :h2)
        books      (first (filter #(str/includes? % "\"books\"") statements))]
    (is (str/includes? books "CREATE TABLE IF NOT EXISTS \"books\""))
    (is (str/includes? books "\"id\" BIGINT NOT NULL"))
    (is (str/includes? books "\"title\" VARCHAR(255) NOT NULL"))
    (is (str/includes? books "\"author_id\" BIGINT"))
    (is (str/includes? books "PRIMARY KEY (\"id\")"))
    (is (str/includes? books "FOREIGN KEY (\"author_id\") REFERENCES \"authors\" (\"id\")"))))

(deftest creates-referenced-tables-first
  (let [statements (sql/ddl model :h2)]
    (is (< (count (take-while #(not (str/includes? % "\"authors\"")) statements))
           (count (take-while #(not (str/includes? % "\"books\"")) statements))))))

(deftest builds-a-select-by-identity
  (let [book (get-in model [:resources :book])]
    (is (= "SELECT \"id\", \"title\", \"author_id\" FROM \"books\" WHERE \"id\" = ?"
           (sql/select-by-identity book)))))

(deftest builds-a-select-with-criteria
  (let [book (get-in model [:resources :book])]
    (is (= ["SELECT \"id\", \"title\", \"author_id\" FROM \"books\" WHERE \"author_id\" = ?" 1]
           (sql/select book {:where {:author-id 1}})))
    (is (= ["SELECT \"id\", \"title\", \"author_id\" FROM \"books\""]
           (sql/select book {})))))

(deftest refuses-criteria-that-are-not-fields
  (let [book (get-in model [:resources :book])]
    (is (thrown? clojure.lang.ExceptionInfo (sql/select book {:where {:sneaky 1}})))))
