(ns hypermedia.bench
  (:require [clojure.string :as str]
            [hypermedia.sql]
            [next.jdbc]
            [hypermedia.api :as api]
            [hypermedia.config :as config]
            [hypermedia.database :as database]
            [hypermedia.store :as store]
            [hypermedia.store.jdbc :as jdbc-store]
            [hypermedia.tally :as tally]
            [jsonista.core :as json]))

(defn- millis [nanos] (/ (double nanos) 1e6))

(defn- percentile [sorted fraction]
  (nth sorted (min (dec (count sorted)) (long (* fraction (count sorted))))))

(defn measure [label runs body]
  (dotimes [_ (max 200 (quot runs 2))] (body))
  (let [samples (long-array runs)]
    (dotimes [n runs]
      (let [started (System/nanoTime)]
        (body)
        (aset samples n (- (System/nanoTime) started))))
    (let [sorted (vec (sort samples))]
      {:label label
       :runs  runs
       :min   (millis (first sorted))
       :p50   (millis (percentile sorted 0.5))
       :p95   (millis (percentile sorted 0.95))
       :rate  (long (/ 1000.0 (max 1e-6 (millis (first sorted)))))})))

(defn report [rows]
  (println (format "%-36s %8s %8s %8s %8s %10s" "case" "runs" "min ms" "p50 ms" "p95 ms" "min/sec"))
  (doseq [{:keys [label runs min p50 p95 rate]} rows]
    (println (format "%-36s %8d %8.3f %8.3f %8.3f %10d" label runs min p50 p95 rate))))

(defn- seeded [handler store model rows]
  (let [writer  (get-in model [:resources :writer])
        work    (get-in model [:resources :work])
        writers (mapv (fn [n]
                        (let [id (random-uuid)]
                          (store/create! store writer {:id id :name (str "writer " n)})
                          id))
                      (range 20))]
    (dotimes [n rows]
      (store/create! store work {:id (random-uuid)
                                 :title (str "title " n)
                                 :year (+ 1900 (mod n 120))
                                 :writer-id (nth writers (mod n 20))}))
    {:handler handler :writers writers}))

(defn- get-in-api [handler path]
  (let [[uri query] (str/split path #"\?" 2)]
    (handler {:request-method :get :uri uri :query-string query :headers {}})))

(defn run
  ([] (run 5000 200))
  ([runs rows]
   (let [engine  (database/h2)
         api*    (config/api "e2e-simple.edn")
         model   (:model api*)
         opened  (jdbc-store/open {:url (:url engine) :model model :migrate? true})
         watched (tally/watching (:datasource opened))
         counted (assoc opened :datasource (:datasource watched))
         handler (api/handler api* counted)]
     (try
       (let [{:keys [writers]} (seeded handler opened model rows)
             any   (:body (json/read-value (:body (get-in-api handler "/works?size=1"))
                                           json/keyword-keys-object-mapper))
             one   (-> (get-in-api handler "/works?size=1") :body
                       (json/read-value json/keyword-keys-object-mapper)
                       (get-in [:_embedded :works 0 :_links :self :href]))]
         (println (format "rows=%d runs=%d" rows runs))
         (report
          [(measure "root" runs #(get-in-api handler "/"))
           (measure "item" runs #(get-in-api handler one))
           (measure "collection size=1" runs #(get-in-api handler "/works?size=1"))
           (measure "collection size=20" runs #(get-in-api handler "/works?size=20"))
           (measure "collection size=100" (quot runs 5) #(get-in-api handler "/works?size=100"))
           (measure "collection size=200" (quot runs 5) #(get-in-api handler "/works?size=200"))
           (measure "collection sorted size=20" runs #(get-in-api handler "/works?size=20&sort=title,asc"))
           (measure "projection size=20" runs #(get-in-api handler "/works?size=20&projection=summary"))
           (measure "first page size=20" (quot runs 5)
                    #(get-in-api handler "/works?size=20&page=0"))
           (measure "last page size=20" (quot runs 5)
                    #(get-in-api handler (str "/works?size=20&page=" (dec (quot rows 20)))))
           (measure "association page" runs
                    #(get-in-api handler (str "/writers/" (first writers) "/works?size=20")))
           (measure "search" runs #(get-in-api handler "/works/search/by-title?title=title%201"))
           (measure "profile" runs #(get-in-api handler "/profile/works"))]))
       (finally (jdbc-store/close opened) ((:stop engine)))))))

(defn -main [& args]
  (run (Long/parseLong (or (first args) "5000"))
       (Long/parseLong (or (second args) "200"))))

(defn- batch-insert [opened resource rows]
  (let [[statement] (hypermedia.sql/insert resource (first rows))
        groups      (mapv (fn [row] (vec (rest (hypermedia.sql/insert resource row)))) rows)]
    (next.jdbc/execute-batch! (:datasource opened) statement groups {})))

(defn- fill [opened model writers-count works-count]
  (let [writer  (get-in model [:resources :writer])
        work    (get-in model [:resources :work])
        writers (mapv (fn [n] {:id (random-uuid) :name (str "writer " n)}) (range writers-count))]
    (doseq [chunk (partition-all 2000 writers)]
      (batch-insert opened writer (vec chunk)))
    (doseq [chunk (partition-all 2000 (range works-count))]
      (batch-insert opened work
                    (mapv (fn [n] {:id (random-uuid)
                                   :title (str "title " n)
                                   :year (+ 1900 (mod n 120))
                                   :writer-id (:id (nth writers (mod n writers-count)))})
                          chunk)))
    {:writers (mapv :id writers)}))

(defn scale
  ([] (scale 100000 400))
  ([rows runs]
   (let [engine  (database/h2)
         api*    (config/api "e2e-simple.edn")
         model   (:model api*)
         opened  (jdbc-store/open {:url (:url engine) :model model :migrate? true})
         handler (api/handler api* opened)]
     (try
       (let [{:keys [writers]} (fill opened model 50 rows)
             pages (quot rows 20)
             one   (-> (get-in-api handler "/works?size=1") :body
                       (json/read-value json/keyword-keys-object-mapper)
                       (get-in [:_embedded :works 0 :_links :self :href]))]
         (println (format "rows=%d runs=%d" rows runs))
         (report
          [(measure "item" runs #(get-in-api handler one))
           (measure "page 0 of 20" runs #(get-in-api handler "/works?size=20&page=0"))
           (measure "page 1/4 of 20" runs
                    #(get-in-api handler (str "/works?size=20&page=" (quot pages 4))))
           (measure "page 1/2 of 20" runs
                    #(get-in-api handler (str "/works?size=20&page=" (quot pages 2))))
           (measure "last page of 20" runs
                    #(get-in-api handler (str "/works?size=20&page=" (dec pages))))
           (measure "page 0 sorted by title" runs
                    #(get-in-api handler "/works?size=20&page=0&sort=title,asc"))
           (measure "last page sorted by title" (quot runs 4)
                    #(get-in-api handler (str "/works?size=20&sort=title,asc&page=" (dec pages))))
           (measure "search by title" runs #(get-in-api handler "/works/search/by-title?title=title%2099"))
           (measure "association page" runs
                    #(get-in-api handler (str "/writers/" (first writers) "/works?size=20")))]))
       (finally (jdbc-store/close opened) ((:stop engine)))))))

(defn- hammer [clients seconds body]
  (let [gate    (java.util.concurrent.CountDownLatch. 1)
        stop    (+ (System/nanoTime) (* seconds 1000000000))
        samples (java.util.concurrent.ConcurrentLinkedQueue.)
        threads (mapv (fn [n]
                        (Thread.
                         ^Runnable
                         (fn []
                           (.await gate)
                           (loop [i n]
                             (when (< (System/nanoTime) stop)
                               (let [started (System/nanoTime)]
                                 (body i)
                                 (.add samples (- (System/nanoTime) started)))
                               (recur (+ i clients)))))))
                      (range clients))]
    (run! #(.start ^Thread %) threads)
    (let [began (System/nanoTime)]
      (.countDown gate)
      (run! #(.join ^Thread %) threads)
      (let [elapsed (/ (double (- (System/nanoTime) began)) 1e9)
            sorted  (vec (sort samples))]
        {:clients clients
         :runs    (count sorted)
         :p50     (millis (percentile sorted 0.5))
         :p95     (millis (percentile sorted 0.95))
         :p99     (millis (percentile sorted 0.99))
         :rate    (long (/ (count sorted) elapsed))}))))

(defn- load-report [rows]
  (println (format "%-30s %8s %8s %8s %8s %10s" "case" "clients" "runs" "p50 ms" "p95 ms" "per sec"))
  (doseq [{:keys [label clients runs p50 p95 rate]} rows]
    (println (format "%-30s %8d %8d %8.3f %8.3f %10d" label clients runs p50 p95 rate))))

(defn under-load
  ([] (under-load 20000 2))
  ([rows seconds]
   (let [engine  (database/h2)
         api*    (config/api "e2e-simple.edn")
         model   (:model api*)
         opened  (jdbc-store/open {:url (:url engine) :model model :migrate? true})
         handler (api/handler api* opened)]
     (try
       (let [{:keys [writers]} (fill opened model 50 rows)
             pages (quot rows 20)]
         (println (format "rows=%d seconds=%d cores=%d" rows seconds
                          (.availableProcessors (Runtime/getRuntime))))
         (hammer 8 seconds
                 (fn [i] (get-in-api handler (str "/works?size=20&page=" (mod i pages)))))
         (load-report
          (for [clients [1 2 4 8 16 32]]
            (assoc (hammer clients seconds
                           (fn [i] (get-in-api handler (str "/works?size=20&page=" (mod i pages)))))
                   :label "page of 20"))))
       (finally (jdbc-store/close opened) ((:stop engine)))))))
