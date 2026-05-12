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

(deftest builds-an-insert
  (let [book (get-in model [:resources :book])]
    (is (= ["INSERT INTO \"books\" (\"id\", \"title\") VALUES (?, ?)" 1 "Dune"]
           (sql/insert book {:id 1 :title "Dune"})))))

(deftest builds-an-update
  (let [book (get-in model [:resources :book])]
    (is (= ["UPDATE \"books\" SET \"title\" = ? WHERE \"id\" = ?" "Dune" 1]
           (sql/update-by-identity book 1 {:title "Dune"})))))

(deftest builds-a-delete
  (let [book (get-in model [:resources :book])]
    (is (= ["DELETE FROM \"books\" WHERE \"id\" = ?" 1]
           (sql/delete-by-identity book 1)))))

(deftest refuses-to-write-fields-outside-the-schema
  (let [book (get-in model [:resources :book])]
    (is (thrown? clojure.lang.ExceptionInfo (sql/insert book {:sneaky 1})))
    (is (thrown? clojure.lang.ExceptionInfo (sql/update-by-identity book 1 {:sneaky 1})))))

(deftest encodes-values-a-dialect-cannot-hold
  (let [id (random-uuid)]
    (is (= id (sql/encode :h2 :uuid id)))
    (is (= (str id) (sql/encode :sqlite :uuid id)))
    (is (= 1 (sql/encode :sqlite :boolean true)))
    (is (= id (sql/decode :sqlite :uuid (str id))))
    (is (true? (sql/decode :sqlite :boolean 1)))))

(deftest pushes-order-and-slice-into-the-statement
  (let [book (get-in model [:resources :book])]
    (is (= ["SELECT \"id\", \"title\", \"author_id\" FROM \"books\" WHERE \"author_id\" = ? ORDER BY \"title\" ASC, \"id\" DESC LIMIT ? OFFSET ?"
            1 5 10]
           (sql/select book {:where {:author-id 1} :order [[:title :asc] [:id :desc]] :limit 5 :offset 10})))))

(deftest refuses-an-order-outside-the-schema
  (is (thrown? clojure.lang.ExceptionInfo
               (sql/select (get-in model [:resources :book]) {:order [[:sneaky :asc]]}))))

(deftest matches-a-set-with-one-predicate
  (let [book (get-in model [:resources :book])]
    (is (= ["SELECT \"id\", \"title\", \"author_id\" FROM \"books\" WHERE \"id\" IN (?, ?)" 1 2]
           (sql/select book {:where {:id #{1 2}}})))
    (is (= ["SELECT \"id\", \"title\", \"author_id\" FROM \"books\" WHERE 1 = 0"]
           (sql/select book {:where {:id #{}}})))
    (is (= ["SELECT COUNT(*) AS \"total\" FROM \"books\" WHERE \"id\" IN (?, ?)" 1 2]
           (sql/count-of book {:where {:id #{1 2}}})))))

(deftest compiles-the-constant-statements-of-a-model
  (let [compiled (sql/statements model)]
    (is (= #{:author :book} (set (keys compiled))))
    (is (= "SELECT \"id\", \"title\", \"author_id\" FROM \"books\" WHERE \"id\" = ?"
           (get-in compiled [:book :by-identity])))
    (is (= "DELETE FROM \"books\" WHERE \"id\" = ?" (get-in compiled [:book :delete])))
    (is (= "SELECT COUNT(*) AS \"total\" FROM \"books\"" (get-in compiled [:book :count])))
    (is (every? string? (vals (:book compiled))))))

(deftest encodes-a-set-element-by-element
  (let [a (random-uuid) b (random-uuid)]
    (is (= #{(str a) (str b)} (set (sql/encode :sqlite :uuid #{a b}))))
    (is (= #{a b} (set (sql/encode :h2 :uuid #{a b}))))
    (is (vector? (sql/encode :sqlite :uuid #{a b})))))

(deftest encodes-for-the-column-the-dialect-uses
  (let [id (random-uuid)]
    (is (= (str id) (sql/encode :derby :uuid id)))
    (is (= (str id) (sql/encode :mysql :uuid id)))
    (is (= id (sql/encode :postgres :uuid id)))
    (is (= id (sql/encode :hsqldb :uuid id)))))

(deftest reads-a-large-object-as-a-value
  (is (= "text" (sql/decode :h2 :text "text")))
  (is (= "text" (sql/decode :h2 :string "text"))))
