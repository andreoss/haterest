(ns hypermedia.concurrency-test
  (:require [clojure.test :refer [deftest is testing]]
            [hypermedia.client :as client]
            [hypermedia.config :as config]
            [hypermedia.database :as database]
            [hypermedia.main :as main]
            [hypermedia.store :as store]
            [hypermedia.store.jdbc :as jdbc-store]))

(def writers 8)

(defn- racing [port path tag]
  (let [gate    (java.util.concurrent.CountDownLatch. 1)
        results (java.util.concurrent.ConcurrentLinkedQueue.)
        threads (mapv (fn [n]
                        (Thread.
                         ^Runnable
                         (fn []
                           (.await gate)
                           (.add results
                                 (:status (client/request port :patch path
                                                          :body (client/json-body {:year (+ 2000 n)})
                                                          :content-type "application/json"
                                                          :headers {"If-Match" tag}))))))
                      (range writers))]
    (run! #(.start ^Thread %) threads)
    (.countDown gate)
    (run! #(.join ^Thread %) threads)
    (vec results)))

(defn- one-winner [engine]
  (let [running (main/start {:schema "e2e-simple.edn" :database (:url engine) :port 0 :migrate true})
        port    (:port running)]
    (try
      (let [created (client/request port :post "/works"
                                    :body (client/json-body {:title "Dune" :year 1965})
                                    :content-type "application/json")
            self    (:location created)
            tag     (:etag (client/request port :get self))
            statuses (racing port self tag)]
        (testing (str (name (:name engine)) " admits one writer of " writers)
          (is (= 1 (count (filter #{200} statuses)))
              (str "statuses were " statuses))
          (is (= (dec writers) (count (filter #{412} statuses)))
              (str "statuses were " statuses))
          (is (empty? (remove #{200 412} statuses))
              (str "statuses were " statuses))))
      (finally ((:stop running))))))

(deftest a-lost-update-is-refused-not-merged
  (let [{:keys [ready declined]} (database/engines)]
    (.println System/err (str "concurrency engines ready: "
                              (clojure.string/join ", " (map (comp name :name) ready))))
    (when (seq declined)
      (.println System/err (str "concurrency engines declined: "
                                (clojure.string/join ", " (map first declined)))))
    (is (seq ready))
    (doseq [engine ready]
      (try (one-winner engine) (finally ((:stop engine)))))))

(deftest a-write-conditions-on-the-version-it-read
  (let [engine (database/h2)
        model  (:model (config/api "e2e-simple.edn"))
        store  (jdbc-store/open {:url (:url engine) :model model :migrate? true})
        work   (get-in model [:resources :work])
        id     (random-uuid)]
    (try
      (store/create! store work {:id id :title "Dune" :year 1965})
      (let [first-read (store/fetch store work id)
            version    (store/version-of first-read)]
        (is (= 0 version))
        (is (= :written (:outcome (store/amend! store work id {:year 1966} version))))
        (is (= 1 (store/version-of (store/fetch store work id))))
        (testing "the version a reader held is refused once someone else has written"
          (is (= :stale (:outcome (store/amend! store work id {:year 1967} version))))
          (is (= 1966 (:year (store/fetch store work id)))))
        (testing "a write without a version is unconditional"
          (is (= :written (:outcome (store/amend! store work id {:year 1968} nil))))))
      (finally (jdbc-store/close store) ((:stop engine))))))

(defn- racing-sets [port roster teams tag]
  (let [gate    (java.util.concurrent.CountDownLatch. 1)
        results (java.util.concurrent.ConcurrentLinkedQueue.)
        threads (mapv (fn [team]
                        (Thread.
                         ^Runnable
                         (fn []
                           (.await gate)
                           (.add results
                                 (:status (client/request port :put roster
                                                          :body team
                                                          :content-type "text/uri-list"
                                                          :headers {"If-Match" tag}))))))
                      teams)]
    (run! #(.start ^Thread %) threads)
    (.countDown gate)
    (run! #(.join ^Thread %) threads)
    (vec results)))

(defn- one-set-wins [engine]
  (let [running (main/start {:schema "e2e-many.edn" :database (:url engine) :port 0 :migrate true})
        port    (:port running)]
    (try
      (let [player (client/request port :post "/players"
                                   :body (client/json-body {:name "Ada"})
                                   :content-type "application/json")
            teams  (mapv (fn [n]
                           (:location (client/request port :post "/teams"
                                                      :body (client/json-body {:name (str "Team " n)})
                                                      :content-type "application/json")))
                         (range writers))
            roster (get-in player [:body :_links :rel:teams :href])
            tag    (:etag (client/request port :get (:location player)))
            statuses (racing-sets port roster teams tag)]
        (testing (str (name (:name engine)) " admits one membership writer of " writers)
          (is (= 1 (count (filter #{204} statuses))) (str "statuses were " statuses))
          (is (= (dec writers) (count (filter #{412} statuses))) (str "statuses were " statuses))
          (is (empty? (remove #{204 412} statuses)) (str "statuses were " statuses))
          (is (= 1 (get-in (client/request port :get roster) [:body :page :totalElements])))))
      (finally ((:stop running))))))

(deftest a-membership-is-written-by-one-client-at-a-time
  (let [{:keys [ready]} (database/engines)]
    (is (seq ready))
    (doseq [engine ready]
      (try (one-set-wins engine) (finally ((:stop engine)))))))

(deftest a-membership-change-changes-the-owner-tag
  (let [engine  (database/h2)
        running (main/start {:schema "e2e-many.edn" :database (:url engine) :port 0 :migrate true})
        port    (:port running)]
    (try
      (let [player (client/request port :post "/players"
                                   :body (client/json-body {:name "Ada"})
                                   :content-type "application/json")
            team   (client/request port :post "/teams"
                                   :body (client/json-body {:name "Blue"})
                                   :content-type "application/json")
            self   (:location player)
            before (:etag (client/request port :get self))]
        (is (= 204 (:status (client/request port :post (get-in player [:body :_links :rel:teams :href])
                                            :body (:location team)
                                            :content-type "text/uri-list"))))
        (let [after (client/request port :get self)]
          (is (not= before (:etag after)))
          (is (= ["Blue"] (map :name (get-in after [:body :_embedded :teams]))))
          (testing "a client holding the old tag is not told nothing changed"
            (is (= 200 (:status (client/request port :get self
                                                :headers {"If-None-Match" before})))))))
      (finally ((:stop running)) ((:stop engine))))))
