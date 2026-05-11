(ns hypermedia.store.jdbc-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [hypermedia.sql :as sql]
            [clojure.test :refer [deftest is testing]]
            [hypermedia.schema :as schema]
            [hypermedia.store :as store]
            [hypermedia.store.jdbc :as jdbc-store]
            [next.jdbc :as jdbc]))

(def model
  (schema/parse
   {:resources
    {:author {:fields    {:id {:type :long :identity true} :name {:type :string :required true}}
              :relations {:books {:kind :has-many :target :book :via :author-id}}}
     :book   {:fields    {:id        {:type :long :identity true}
                          :title     {:type :string :required true}
                          :author-id {:type :long}}
              :relations {:author {:kind :belongs-to :target :author :via :author-id}}}}}))

(defn- seed [opened]
  (let [ds (:datasource opened)]
    (jdbc/execute! ds ["INSERT INTO \"authors\" (\"id\", \"name\") VALUES (1, 'Herbert')"])
    (jdbc/execute! ds ["INSERT INTO \"books\" (\"id\", \"title\", \"author_id\") VALUES (1, 'Dune', 1)"])
    (jdbc/execute! ds ["INSERT INTO \"books\" (\"id\", \"title\", \"author_id\") VALUES (2, 'Messiah', 1)"])
    opened))

(defn- urls []
  [(str "jdbc:h2:mem:" (gensym "store") ";DB_CLOSE_DELAY=-1")
   (str "jdbc:sqlite:scratch/" (gensym "store") ".db")])

(defn- exercise [url check]
  (.mkdirs (io/file "scratch"))
  (let [opened (seed (jdbc-store/open {:url url :model model :migrate? true}))]
    (try (check opened)
         (finally
           (jdbc-store/close opened)
           (when-let [path (second (re-find #"^jdbc:sqlite:(.+)$" url))]
             (io/delete-file path true))))))

(deftest reads-through-the-port
  (doseq [url (urls)]
    (testing url
      (exercise url
                (fn [s]
                  (let [book (get-in model [:resources :book])]
                    (is (= "Dune" (:title (store/fetch s book 1))))
                    (is (nil? (store/fetch s book 99)))
                    (is (= 1 (:author-id (store/fetch s book 1))))
                    (is (= 2 (count (store/query s book {}))))
                    (is (= ["Dune"] (map :title (store/query s book {:where {:title "Dune"}}))))
                    (is (= 2 (store/total s book {:where {:author-id 1}})))))))))

(deftest orders-and-slices-in-sql
  (doseq [url (urls)]
    (testing url
      (exercise url
                (fn [s]
                  (let [book (get-in model [:resources :book])]
                    (is (= ["Messiah" "Dune"]
                           (map :title (store/query s book {:order [[:title :desc]]}))))
                    (is (= ["Messiah"]
                           (map :title (store/query s book {:order [[:title :asc]] :limit 1 :offset 1}))))))))))

(deftest probes-the-database
  (doseq [url (urls)]
    (testing url
      (exercise url (fn [s] (is (true? (store/probe s))))))))

(deftest a-closed-store-fails-its-probe
  (let [url    (str "jdbc:h2:mem:" (gensym "store") ";DB_CLOSE_DELAY=0")
        opened (jdbc-store/open {:url url :model model :migrate? true})]
    (jdbc-store/close opened)
    (is (false? (store/probe opened)))))

(deftest refuses-criteria-outside-the-schema
  (exercise (first (urls))
            (fn [s]
              (is (thrown? clojure.lang.ExceptionInfo
                           (store/query s (get-in model [:resources :book]) {:where {:drop-table 1}}))))))

(deftest writes-through-the-port
  (doseq [url (urls)]
    (testing url
      (exercise url
                (fn [s]
                  (let [book (get-in model [:resources :book])]
                    (is (= "Emma" (:title (store/create! s book {:id 3 :title "Emma" :author-id 1}))))
                    (is (= 3 (store/total s book {})))
                    (let [{:keys [created? row]} (store/replace! s book 3 {:title "Persuasion"})]
                      (is (false? created?))
                      (is (= "Persuasion" (:title row)))
                      (is (nil? (:author-id row))))
                    (let [{:keys [created?]} (store/replace! s book 4 {:title "Emma"})]
                      (is (true? created?)))
                    (is (= "Emma!" (:title (store/amend! s book 4 {:title "Emma!"}))))
                    (is (nil? (store/amend! s book 99 {:title "x"})))
                    (is (true? (store/erase! s book 4)))
                    (is (false? (store/erase! s book 4)))))))))

(deftest a-transaction-is-rolled-back-on-failure
  (doseq [url (urls)]
    (testing url
      (exercise url
                (fn [s]
                  (let [book (get-in model [:resources :book])]
                    (is (thrown? Exception
                                 (store/transact s (fn [tx]
                                                     (store/create! tx book {:id 9 :title "Ghost"})
                                                     (throw (ex-info "abandon" {}))))))
                    (is (nil? (store/fetch s book 9)))))))))

(def shaped
  (schema/parse
   {:resources
    {:record {:fields {:id     {:type :uuid :identity true :generated true}
                       :done   {:type :boolean}
                       :at     {:type :instant}
                       :on     {:type :date}
                       :amount {:type :decimal}}}}}))

(deftest carries-values-a-dialect-cannot-hold-natively
  (doseq [url [(str "jdbc:h2:mem:" (gensym "store") ";DB_CLOSE_DELAY=-1")
               (str "jdbc:sqlite:scratch/" (gensym "store") ".db")]]
    (testing url
      (.mkdirs (io/file "scratch"))
      (let [opened (jdbc-store/open {:url url :model shaped :migrate? true})
            record (get-in shaped [:resources :record])
            id     (random-uuid)
            at     (java.time.Instant/parse "2026-09-25T10:00:00Z")
            on     (java.time.LocalDate/parse "2026-09-25")]
        (try
          (store/create! opened record {:id id :done true :at at :on on})
          (let [row (store/fetch opened record id)]
            (is (= id (:id row)))
            (is (true? (:done row)))
            (is (= at (:at row)))
            (is (= on (:on row))))
          (is (= 1 (store/total opened record {:where {:id id}})))
          (finally
            (jdbc-store/close opened)
            (when-let [path (second (re-find #"^jdbc:sqlite:(.+)$" url))]
              (io/delete-file path true))))))))

(def joined
  (schema/parse
   {:resources
    {:author {:fields    {:id {:type :long :identity true} :name {:type :string :required true}}
              :relations {:books {:kind :many-to-many :target :book :through :authorship
                                  :via :author-id :target-via :book-id}}}
     :book   {:fields    {:id {:type :long :identity true} :title {:type :string :required true}}
              :relations {:authors {:kind :many-to-many :target :author :through :authorship
                                    :via :book-id :target-via :author-id}}}}}))

(deftest derives-one-join-table-for-both-sides
  (let [statements (sql/ddl joined :h2)
        join       (first (filter #(str/includes? % "\"authorship\"") statements))]
    (is (= 3 (count statements)))
    (is (= (count statements) (count (distinct statements))))
    (is (str/includes? join "\"author_id\" BIGINT NOT NULL"))
    (is (str/includes? join "\"book_id\" BIGINT NOT NULL"))
    (is (str/includes? join "PRIMARY KEY (\"author_id\", \"book_id\")"))
    (is (str/includes? join "FOREIGN KEY (\"author_id\") REFERENCES \"authors\" (\"id\")"))
    (is (str/includes? join "FOREIGN KEY (\"book_id\") REFERENCES \"books\" (\"id\")"))
    (is (= (last statements) join))))

(defn- with-join [url check]
  (.mkdirs (io/file "scratch"))
  (let [opened (jdbc-store/open {:url url :model joined :migrate? true})
        ds     (:datasource opened)]
    (try
      (jdbc/execute! ds ["INSERT INTO \"authors\" (\"id\", \"name\") VALUES (1, 'Herbert')"])
      (jdbc/execute! ds ["INSERT INTO \"authors\" (\"id\", \"name\") VALUES (2, 'Anderson')"])
      (jdbc/execute! ds ["INSERT INTO \"books\" (\"id\", \"title\") VALUES (1, 'Dune')"])
      (jdbc/execute! ds ["INSERT INTO \"books\" (\"id\", \"title\") VALUES (2, 'Sandworms')"])
      (check opened)
      (finally
        (jdbc-store/close opened)
        (when-let [path (second (re-find #"^jdbc:sqlite:(.+)$" url))]
          (io/delete-file path true))))))

(deftest traverses-and-writes-a-join
  (doseq [url (urls)]
    (testing url
      (with-join
        url
        (fn [s]
          (let [author   (get-in joined [:resources :author])
                book     (get-in joined [:resources :book])
                books    (get-in joined [:resources :author :relations :books])
                authors  (get-in joined [:resources :book :relations :authors])]
            (is (= 2 (store/link! s author book books 1 [1 2])))
            (is (= 1 (store/link! s book author authors 2 [2])))
            (is (= ["Dune" "Sandworms"]
                   (map :title (store/linked s author book books 1 {:order [[:title :asc]]}))))
            (is (= ["Sandworms"]
                   (map :title (store/linked s author book books 1
                                             {:order [[:title :asc]] :limit 1 :offset 1}))))
            (is (= 2 (store/linked-total s author book books 1)))
            (is (= ["Anderson" "Herbert"]
                   (sort (map :name (store/linked s book author authors 2 {})))))
            (is (= [[1 1] [1 2]] (store/links-of s author book books [1])))
            (is (= 1 (store/unlink! s author book books 1 [1])))
            (is (= 1 (store/linked-total s author book books 1)))
            (is (= 0 (store/unlink! s author book books 1 [1])))
            (is (= 1 (store/unlink! s author book books 1 nil)))
            (is (= 0 (store/linked-total s author book books 1)))))))))

(deftest carries-join-keys-a-dialect-cannot-hold-natively
  (let [model (schema/parse
               {:resources
                {:tag  {:fields    {:id {:type :uuid :identity true} :name {:type :string}}
                        :relations {:notes {:kind :many-to-many :target :note :through :tagging
                                            :via :tag-id :target-via :note-id}}}
                 :note {:fields {:id {:type :uuid :identity true} :body {:type :string}}}}})
        url   (str "jdbc:sqlite:scratch/" (gensym "store") ".db")]
    (.mkdirs (io/file "scratch"))
    (let [opened  (jdbc-store/open {:url url :model model :migrate? true})
          tag     (get-in model [:resources :tag])
          note    (get-in model [:resources :note])
          notes   (get-in model [:resources :tag :relations :notes])
          tag-id  (random-uuid)
          note-id (random-uuid)]
      (try
        (store/create! opened tag {:id tag-id :name "epic"})
        (store/create! opened note {:id note-id :body "a note"})
        (store/link! opened tag note notes tag-id [note-id])
        (is (= [note-id] (map :id (store/linked opened tag note notes tag-id {}))))
        (is (= [[tag-id note-id]] (store/links-of opened tag note notes [tag-id])))
        (is (= 1 (store/unlink! opened tag note notes tag-id [note-id])))
        (finally
          (jdbc-store/close opened)
          (io/delete-file (second (re-find #"^jdbc:sqlite:(.+)$" url)) true))))))
