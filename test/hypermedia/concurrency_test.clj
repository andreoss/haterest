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
