(ns hypermedia.main-test
  (:require [clojure.test :refer [deftest is]]
            [hypermedia.client :as client]
            [hypermedia.main :as main]))

(defn- request
  ([port method path] (client/request port method path))
  ([port method path payload]
   (client/request port method path
                   :body (client/json-body payload)
                   :content-type "application/json")))


(defn- serving [body]
  (let [running (main/start {:schema   "example.edn"
                             :database (str "jdbc:h2:mem:" (gensym "main") ";DB_CLOSE_DELAY=-1")
                             :port     0
                             :migrate  true})]
    (try (body running) (finally ((:stop running))))))

(deftest parses-options
  (let [options (main/options ["-s" "example.edn" "-d" "jdbc:h2:mem:x" "-p" "0"])]
    (is (= "example.edn" (:schema options)))
    (is (= "jdbc:h2:mem:x" (:database options)))
    (is (= 0 (:port options)))))

(deftest requires-a-schema-and-a-database
  (is (seq (main/problems (main/options []))))
  (is (empty? (main/problems (main/options ["-s" "a" "-d" "b"])))))

(deftest boots-from-a-schema-and-a-url
  (serving
   (fn [running]
     (let [port (:port running)]
       (is (pos? port))
       (is (= "/books" (get-in (request port :get "/") [:body :_links :rel:books :href])))
       (is (= 200 (:status (request port :get "/health"))))
       (is (= "up" (get-in (request port :get "/health") [:body :status])))
       (is (= [] (get-in (request port :get "/books") [:body :_embedded :books])))))))

(deftest a-client-navigates-and-writes-by-link-alone
  (serving
   (fn [running]
     (let [port    (:port running)
           root    (:body (request port :get "/"))
           authors (get-in root [:_links :rel:authors :href])
           books   (get-in root [:_links :rel:books :href])
           author  (request port :post authors {:name "Herbert"})
           book    (request port :post books {:title "Dune" :year 1965 :author (:location author)})]
       (is (= 201 (:status author)))
       (is (= 201 (:status book)))
       (is (= "Herbert" (get-in (request port :get (get-in book [:body :_links :rel:author :href]))
                                [:body :name])))
       (let [owned (get-in (request port :get (get-in author [:body :_links :rel:books :href])) [:body])]
         (is (= ["Dune"] (map :title (get-in owned [:_embedded :books])))))
       (is (= 1966 (get-in (request port :patch (:location book) {:year 1966}) [:body :year])))
       (is (= 204 (:status (request port :delete (:location book)))))
       (is (= 404 (:status (request port :get (:location book)))))))))

(deftest reports-an-unreachable-store-as-unhealthy
  (serving
   (fn [running]
     (main/detach running)
     (is (= 503 (:status (request (:port running) :get "/health")))))))

(deftest the-pool-is-an-operator-decision
  (let [options (main/options ["-s" "a" "-d" "b" "-c" "24" "--connection-timeout" "2000"])]
    (is (= 24 (:connections options)))
    (is (= 2000 (:connection-timeout options))))
  (is (nil? (:connections (main/options ["-s" "a" "-d" "b"])))))

(deftest a-store-it-cannot-reach-is-said-plainly
  (let [failed (main/refusal (java.net.ConnectException. "Connection refused")
                             {:database "jdbc:postgresql://127.0.0.1:1/nope"})]
    (is (= 4 (:code failed)))
    (is (= "cannot reach the store at jdbc:postgresql://127.0.0.1:1/nope" (first (:lines failed))))
    (is (clojure.string/includes? (second (:lines failed)) "Connection refused"))))

(deftest a-schema-that-is-not-there-is-said-plainly
  (let [failed (main/refusal (ex-info "schema not found" {:type :hypermedia.config/not-found})
                             {:schema "nowhere.edn"})]
    (is (= 2 (:code failed)))
    (is (= ["no schema at nowhere.edn"] (:lines failed)))))

(deftest a-store-that-cannot-be-evolved-is-said-plainly
  (let [failed (main/refusal (ex-info "refused" {:type :hypermedia.store.jdbc/refused
                                                 :refusals [{:table :t :column :c :reason :type-differs}]})
                             {})]
    (is (= 3 (:code failed)))
    (is (= 1 (count (:refusals failed)))))
  (let [failed (main/refusal (ex-info "unfinished" {:type :hypermedia.store.jdbc/unfinished
                                                    :failures [{:message "lock timed out"}]})
                             {})]
    (is (= 3 (:code failed)))
    (is (some #(= "lock timed out" %) (:lines failed)))))

(deftest the-drain-and-the-log-are-operator-decisions
  (let [options (main/options ["-s" "a" "-d" "b" "--quiet" "--drain" "2500"])]
    (is (true? (:quiet options)))
    (is (= 2500 (:drain options))))
  (is (false? (:quiet (main/options ["-s" "a" "-d" "b"])))))
