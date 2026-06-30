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
    (is (= "SELECT \"id\", \"title\", \"author_id\", \"row_version\" FROM \"books\" WHERE \"id\" = ?"
           (sql/select-by-identity book)))))

(deftest builds-a-select-with-criteria
  (let [book (get-in model [:resources :book])]
    (is (= ["SELECT \"id\", \"title\", \"author_id\", \"row_version\" FROM \"books\" WHERE \"author_id\" = ? ORDER BY \"id\" ASC" 1]
           (sql/select book {:where {:author-id 1}})))
    (is (= ["SELECT \"id\", \"title\", \"author_id\", \"row_version\" FROM \"books\" ORDER BY \"id\" ASC"]
           (sql/select book {})))))

(deftest refuses-criteria-that-are-not-fields
  (let [book (get-in model [:resources :book])]
    (is (thrown? clojure.lang.ExceptionInfo (sql/select book {:where {:sneaky 1}})))))

(deftest builds-an-insert
  (let [book (get-in model [:resources :book])]
    (is (= ["INSERT INTO \"books\" (\"id\", \"title\", \"row_version\") VALUES (?, ?, ?)" 1 "Dune" 0]
           (sql/insert book {:id 1 :title "Dune"})))))

(deftest builds-an-update
  (let [book (get-in model [:resources :book])]
    (is (= ["UPDATE \"books\" SET \"title\" = ?, \"row_version\" = \"row_version\" + 1 WHERE \"id\" = ?"
            "Dune" 1]
           (sql/update-by-identity book 1 {:title "Dune"})))
    (is (= ["UPDATE \"books\" SET \"title\" = ?, \"row_version\" = \"row_version\" + 1 WHERE \"id\" = ? AND \"row_version\" = ?"
            "Dune" 1 3]
           (sql/update-by-identity book 1 {:title "Dune"} 3)))
    (is (= ["DELETE FROM \"books\" WHERE \"id\" = ? AND \"row_version\" = ?" 1 3]
           (sql/delete-by-identity book 1 3)))))

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
    (is (= ["SELECT \"id\", \"title\", \"author_id\", \"row_version\" FROM \"books\" WHERE \"author_id\" = ? ORDER BY \"title\" ASC, \"id\" DESC LIMIT ? OFFSET ?"
            1 5 10]
           (sql/select book {:where {:author-id 1} :order [[:title :asc] [:id :desc]] :limit 5 :offset 10})))))

(deftest refuses-an-order-outside-the-schema
  (is (thrown? clojure.lang.ExceptionInfo
               (sql/select (get-in model [:resources :book]) {:order [[:sneaky :asc]]}))))

(deftest matches-a-set-with-one-predicate
  (let [book (get-in model [:resources :book])]
    (is (= ["SELECT \"id\", \"title\", \"author_id\", \"row_version\" FROM \"books\" WHERE \"id\" IN (?, ?) ORDER BY \"id\" ASC" 1 2]
           (sql/select book {:where {:id #{1 2}}})))
    (is (= ["SELECT \"id\", \"title\", \"author_id\", \"row_version\" FROM \"books\" WHERE 1 = 0 ORDER BY \"id\" ASC"]
           (sql/select book {:where {:id #{}}})))
    (is (= ["SELECT COUNT(*) AS \"total\" FROM \"books\" WHERE \"id\" IN (?, ?)" 1 2]
           (sql/count-of book {:where {:id #{1 2}}})))))

(deftest compiles-the-constant-statements-of-a-model
  (let [compiled (sql/statements model)]
    (is (= #{:author :book} (set (keys compiled))))
    (is (= "SELECT \"id\", \"title\", \"author_id\", \"row_version\" FROM \"books\" WHERE \"id\" = ?"
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

(deftest builds-a-bulk-update
  (let [book (get-in model [:resources :book])]
    (is (= ["UPDATE \"books\" SET \"author_id\" = ?, \"row_version\" = \"row_version\" + 1 WHERE \"author_id\" = ?"
            nil 1]
           (sql/update-where book {:author-id 1} {:author-id nil})))
    (is (= ["UPDATE \"books\" SET \"author_id\" = ?, \"row_version\" = \"row_version\" + 1 WHERE \"id\" IN (?, ?)"
            7 1 2]
           (sql/update-where book {:id #{1 2}} {:author-id 7})))))

(def searchable
  (schema/parse
   {:resources
    {:author {:fields   {:id {:type :long :identity true} :name {:type :string}}
              :searches {:by-name {:predicates [:name]}}
              :relations {:books {:kind :has-many :target :book :via :author-id}}}
     :book   {:fields      {:id        {:type :long :identity true}
                            :title     {:type :string}
                            :year      {:type :long :indexed true}
                            :author-id {:type :long}}
              :relations   {:author {:kind :belongs-to :target :author :via :author-id}}
              :searches    {:by-title {:predicates [:title]}}}}}))

(deftest indexes-what-the-schema-says-it-filters-on
  (let [columns (sql/indexed-columns searchable :postgres)]
    (is (= #{:name} (get columns :authors)))
    (is (= #{:author_id :title :year} (get columns :books)))))

(deftest a-foreign-key-is-left-to-an-engine-that-indexes-it
  (is (= #{:title :year} (get (sql/indexed-columns searchable :h2) :books)))
  (is (contains? (get (sql/indexed-columns searchable :sqlite) :books) :author_id)))

(deftest an-identity-is-not-indexed-twice
  (is (not (contains? (get (sql/indexed-columns searchable :postgres) :books) :id))))

(deftest indexes-are-named-for-what-they-cover
  (let [statements (sql/indexes searchable :h2)]
    (is (= 3 (count statements)))
    (is (some #(= "CREATE INDEX IF NOT EXISTS \"ix_books_title\" ON \"books\" (\"title\")" %) statements))
    (is (some #(= "CREATE INDEX IF NOT EXISTS \"ix_books_author_id\" ON \"books\" (\"author_id\")" %)
              (sql/indexes searchable :postgres)))
    (is (every? #(str/starts-with? % "CREATE INDEX") statements))))

(deftest a-dialect-without-the-clause-states-the-index-plainly
  (is (every? #(str/starts-with? % "CREATE INDEX \"ix_") (sql/indexes searchable :derby))))

(deftest a-join-is-indexed-in-both-directions
  (let [model (schema/parse
               {:resources
                {:a {:fields {:id {:type :long :identity true}}
                     :relations {:bs {:kind :many-to-many :target :b :through :ab
                                      :via :a-id :target-via :b-id}}}
                 :b {:fields {:id {:type :long :identity true}}}}})]
    (is (= #{:b_id} (get (sql/indexed-columns model :h2) :ab)))))

(deftest an-ordering-is-made-total
  (let [book (get-in model [:resources :book])]
    (is (= [[:title :asc] [:id :asc]] (sql/total-order book [[:title :asc]])))
    (is (= [[:id :desc]] (sql/total-order book [[:id :desc]])))
    (is (= [[:id :asc]] (sql/total-order book [])))))

(deftest a-sort-on-a-column-that-repeats-still-orders-every-row
  (let [book (get-in model [:resources :book])]
    (is (str/includes? (first (sql/select book {:order [[:title :asc]]}))
                       "ORDER BY \"title\" ASC, \"id\" ASC"))))

(deftest continues-after-a-row-instead-of-counting-past-it
  (let [book (get-in model [:resources :book])]
    (is (= ["SELECT \"id\", \"title\", \"author_id\", \"row_version\" FROM \"books\" WHERE ((\"id\" > ?)) ORDER BY \"id\" ASC LIMIT ?"
            7 20]
           (sql/select book {:order [] :after [7] :limit 20 :offset 9999})))))

(deftest a-mixed-ordering-continues-in-the-right-direction
  (let [book   (get-in model [:resources :book])
        [sql & params] (sql/select book {:order [[:title :desc]] :after ["m" 3] :limit 5})]
    (is (str/includes? sql "((\"title\" < ?) OR (\"title\" = ? AND \"id\" > ?))"))
    (is (= ["m" "m" 3 5] params))))

(deftest an-offset-is-not-used-once-a-cursor-is
  (let [book (get-in model [:resources :book])]
    (is (not (str/includes? (first (sql/select book {:after [1] :limit 5 :offset 500})) "OFFSET")))
    (is (str/includes? (first (sql/select book {:limit 5 :offset 500})) "OFFSET"))))

(deftest a-uniform-ordering-continues-by-row-where-the-dialect-allows
  (let [book (get-in model [:resources :book])
        [sql & params] (sql/select :h2 book {:order [[:title :asc]] :after ["m" 3] :limit 5})]
    (is (str/includes? sql "(\"title\", \"id\") > (?, ?)"))
    (is (= ["m" 3 5] params))))

(deftest a-dialect-without-row-values-spells-it-out
  (let [book (get-in model [:resources :book])
        [sql] (sql/select :derby book {:order [[:title :asc]] :after ["m" 3] :limit 5})]
    (is (str/includes? sql "((\"title\" > ?) OR (\"title\" = ? AND \"id\" > ?))"))))

(deftest a-mixed-ordering-is-spelled-out-even-where-rows-compare
  (let [book (get-in model [:resources :book])
        [sql] (sql/select :h2 book {:order [[:title :desc]] :after ["m" 3] :limit 5})]
    (is (str/includes? sql "((\"title\" < ?) OR (\"title\" = ? AND \"id\" > ?))"))))

(deftest a-descending-pair-continues-downwards
  (let [book (get-in model [:resources :book])
        [sql] (sql/select :h2 book {:order [[:title :desc] [:id :desc]] :after ["m" 3] :limit 5})]
    (is (str/includes? sql "(\"title\", \"id\") < (?, ?)"))))
