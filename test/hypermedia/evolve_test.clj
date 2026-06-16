(ns hypermedia.evolve-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hypermedia.client :as client]
            [hypermedia.config :as config]
            [hypermedia.database :as database]
            [hypermedia.evolve :as evolve]
            [hypermedia.main :as main]
            [hypermedia.sql :as sql]
            [hypermedia.store.jdbc :as jdbc-store]))

(defn- opened [engine schema migrate?]
  (jdbc-store/open {:url (:url engine) :model (:model (config/api schema)) :migrate? migrate?}))

(defn- plan-for [engine schema]
  (let [store (opened engine schema false)]
    (try (jdbc-store/evolution (:datasource store)
                               (:model (config/api schema))
                               (sql/dialect (:url engine)))
         (finally (jdbc-store/close store)))))

(defn- serving [engine schema body]
  (let [running (main/start {:schema schema :database (:url engine) :port 0 :migrate true})]
    (try (body (:port running)) (finally ((:stop running))))))

(deftest a-store-made-from-a-schema-matches-that-schema
  (let [{:keys [ready declined]} (database/engines)]
    (.println System/err (str "evolve engines ready: "
                              (str/join ", " (map (comp name :name) ready))))
    (when (seq declined)
      (.println System/err (str "evolve engines declined: " (str/join ", " (map first declined)))))
    (is (seq ready))
    (doseq [engine ready]
      (try
        (testing (str (name (:name engine)) " reports nothing to do against its own schema")
          (let [store (opened engine "evolve-after.edn" true)]
            (try
              (let [{:keys [statements refusals notes]}
                    (jdbc-store/evolution (:datasource store)
                                          (:model (config/api "evolve-after.edn"))
                                          (sql/dialect (:url engine)))]
                (is (empty? refusals) (str "refusals were " refusals))
                (is (empty? statements) (str "statements were " statements))
                (is (empty? notes) (str "notes were " notes)))
              (finally (jdbc-store/close store)))))
        (finally ((:stop engine)))))))

(deftest a-widened-schema-is-applied-and-keeps-what-was-stored
  (let [{:keys [ready]} (database/engines)]
    (is (seq ready))
    (doseq [engine ready]
      (try
        (testing (name (:name engine))
          (let [kept (atom nil)]
            (serving engine "evolve-before.edn"
                     (fn [port]
                       (let [made (client/request port :post "/notes"
                                                  :body (client/json-body {:body "written before"})
                                                  :content-type "application/json")]
                         (is (= 201 (:status made)))
                         (reset! kept (:location made)))))
            (serving engine "evolve-after.edn"
                     (fn [port]
                       (testing "the row written before is still there"
                         (let [read-back (client/request port :get @kept)]
                           (is (= 200 (:status read-back)))
                           (is (= "written before" (get-in read-back [:body :body])))))
                       (testing "the fields added since are usable"
                         (let [amended (client/request port :patch @kept
                                                       :body (client/json-body {:tag "t" :count 7})
                                                       :content-type "application/json")]
                           (is (= 200 (:status amended)))
                           (is (= "t" (get-in amended [:body :tag])))
                           (is (= 7 (get-in amended [:body :count])))))
                       (testing "a resource added since is served"
                         (is (= 201 (:status (client/request port :post "/labels"
                                                             :body (client/json-body {:name "n"})
                                                             :content-type "application/json")))))
                       (testing "a search added since is answered"
                         (is (= 1 (get-in (client/request port :get "/notes/search/by-tag?tag=t")
                                          [:body :page :totalElements]))))))))
        (finally ((:stop engine)))))))

(deftest a-schema-that-would-lose-something-is-refused
  (let [{:keys [ready]} (database/engines)]
    (is (seq ready))
    (doseq [engine ready]
      (try
        (testing (name (:name engine))
          (let [store (opened engine "evolve-after.edn" true)]
            (jdbc-store/close store))
          (testing "a field the schema no longer declares is reported, not dropped"
            (let [{:keys [refusals notes]} (plan-for engine "evolve-shrunk.edn")]
              (is (some #(= :required-and-not-declared (:reason %)) refusals)
                  (str "refusals were " refusals))
              (is (some #(= :not-declared (:reason %)) notes)
                  (str "notes were " notes))))
          (testing "a field whose type changed is refused"
            (let [{:keys [refusals]} (plan-for engine "evolve-retyped.edn")]
              (is (some #(= :type-differs (:reason %)) refusals)
                  (str "refusals were " refusals))))
          (testing "the store refuses to open rather than serve a schema it cannot meet"
            (is (thrown? clojure.lang.ExceptionInfo (opened engine "evolve-retyped.edn" true)))))
        (finally ((:stop engine)))))))
