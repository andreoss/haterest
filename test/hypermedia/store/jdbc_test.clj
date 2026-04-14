(ns hypermedia.store.jdbc-test
  (:require [clojure.java.io :as io]
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
