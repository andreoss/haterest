(ns hypermedia.budget-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hypermedia.api :as api]
            [hypermedia.config :as config]
            [hypermedia.database :as database]
            [hypermedia.store :as store]
            [hypermedia.store.jdbc :as jdbc-store]
            [hypermedia.tally :as tally]
            [jsonista.core :as json]))

(defn- with-api [body]
  (let [engine  (database/h2)
        api*    (config/api "e2e-simple.edn")
        model   (:model api*)
        opened  (jdbc-store/open {:url (:url engine) :model model :migrate? true})
        watched (tally/watching (:datasource opened))
        handler (api/handler api* (assoc opened :datasource (:datasource watched)))
        writer  (get-in model [:resources :writer])
        work    (get-in model [:resources :work])
        writers (mapv (fn [n]
                        (let [id (random-uuid)]
                          (store/create! opened writer {:id id :name (str "writer " n)})
                          id))
                      (range 10))
        works   (mapv (fn [n]
                        (let [id (random-uuid)]
                          (store/create! opened work {:id id :title (str "title " n) :year (+ 1900 n)
                                                      :writer-id (nth writers (mod n 10))})
                          id))
                      (range 60))]
    (try (body {:handler handler :watched watched :writers writers :works works})
         (finally (jdbc-store/close opened) ((:stop engine))))))

(defn- statements [{:keys [handler watched]} method path & [body content-type]]
  (tally/reset! watched)
  (let [[uri query] (str/split path #"\?" 2)]
    (handler (cond-> {:request-method method :uri uri :query-string query :headers {}}
               body (assoc :body body :headers {"content-type" content-type})))
    (tally/count-of watched)))

(deftest a-request-of-each-kind-costs-what-it-should
  (with-api
   (fn [{:keys [writers works] :as running}]
     (let [work-self   (str "/works/" (first works))
           writer-self (str "/writers/" (first writers))]
       (testing "a read that touches no resource touches no statement"
         (is (= 0 (statements running :get "/")))
         (is (= 0 (statements running :get "/profile")))
         (is (= 0 (statements running :get "/profile/works"))))

       (testing "a page costs its rows, its count and one query per embedded relation"
         (is (>= 3 (statements running :get "/works?size=20")))
         (is (>= 2 (statements running :get "/works?size=20&projection=summary"))))

       (testing "an item costs its row and one query per embedded relation"
         (is (>= 2 (statements running :get work-self)))
         (is (>= 1 (statements running :get writer-self))))

       (testing "an association costs its owner, its rows, its count and its embedding"
         (is (>= 4 (statements running :get (str writer-self "/works?size=20"))))
         (is (>= 2 (statements running :get (str work-self "/writer")))))

       (testing "a search costs what a page costs"
         (is (>= 3 (statements running :get "/works/search/by-title?title=title%201"))))

       (testing "a write costs its statement, what it must read to guard it, and the read back"
         (is (>= 2 (statements running :post "/works"
                               (json/write-value-as-string {:title "new"}) "application/json")))
         (is (>= 4 (statements running :patch work-self
                               (json/write-value-as-string {:year 2001}) "application/json")))
         (is (>= 4 (statements running :put (str (str "/works/" (second works)) "/writer")
                               (str "/writers/" (second writers)) "text/uri-list")))
         (is (>= 2 (statements running :delete (str "/works/" (last works))))))))))

(deftest a-page-costs-the-same-whatever-it-holds
  (with-api
   (fn [{:keys [writers] :as running}]
     (testing "a collection does not pay per row"
       (is (>= (statements running :get "/works?size=1")
               (statements running :get "/works?size=60"))))
     (testing "an association does not pay per row"
       (is (>= (statements running :get (str "/writers/" (first writers) "/works?size=1"))
               (statements running :get (str "/writers/" (first writers) "/works?size=60")))))
     (testing "a reference list does not pay per reference"
       (let [one  (statements running :put (str "/writers/" (first writers) "/works")
                              (str "/works/" (first (:works running))) "text/uri-list")
             many (statements running :put (str "/writers/" (second writers) "/works")
                              (str/join "\n" (map #(str "/works/" %) (take 20 (:works running))))
                              "text/uri-list")]
         (is (>= 6 one) (str "one reference cost " one))
         (is (< (- many one) 21)
             (str "one reference cost " one " and twenty cost " many)))))))

(defn- with-many [body]
  (let [engine  (database/h2)
        api*    (config/api "e2e-many.edn")
        model   (:model api*)
        opened  (jdbc-store/open {:url (:url engine) :model model :migrate? true})
        watched (tally/watching (:datasource opened))
        handler (api/handler api* (assoc opened :datasource (:datasource watched)))
        player  (get-in model [:resources :player])
        team    (get-in model [:resources :team])
        players (mapv (fn [n]
                        (let [id (random-uuid)]
                          (store/create! opened player {:id id :name (str "player " n)})
                          id))
                      (range 4))
        teams   (mapv (fn [n]
                        (let [id (random-uuid)]
                          (store/create! opened team {:id id :name (str "team " n)})
                          id))
                      (range 40))]
    (try (body {:handler handler :watched watched :players players :teams teams})
         (finally (jdbc-store/close opened) ((:stop engine))))))

(deftest a-membership-costs-the-same-whatever-it-holds
  (with-many
   (fn [{:keys [players teams] :as running}]
     (let [roster (fn [n] (str "/players/" (nth players n) "/teams"))
           uris   (fn [n] (str/join "\n" (map #(str "/teams/" %) (take n teams))))
           put    (fn [n count] (statements running :put (roster n) (uris count) "text/uri-list"))]
       (testing "writing a membership does not pay per member"
         (let [one  (put 0 1)
               many (put 1 40)]
           (is (>= 8 one) (str "one member cost " one))
           (is (>= 2 (- many one))
               (str "one member cost " one " and forty cost " many))))
       (testing "reading a membership does not pay per member"
         (is (= (statements running :get (str (roster 1) "?size=1"))
                (statements running :get (str (roster 1) "?size=40")))))
       (testing "a page embedding a membership does not pay per owner"
         (is (= (statements running :get "/players?size=1")
                (statements running :get "/players?size=4"))))))))

(deftest a-page-that-tells-its-own-total-does-not-ask-for-one
  (with-api
   (fn [{:keys [writers] :as running}]
     (testing "a page that is not full states the total it already knows"
       (is (>= 2 (statements running :get "/works?size=100")))
       (is (>= 2 (statements running :get "/works?size=50&page=1"))))
     (testing "a full page still asks"
       (is (>= 3 (statements running :get "/works?size=20&page=0"))))
     (testing "a page past the end still asks, because it knows nothing"
       (is (>= 3 (statements running :get "/works?size=20&page=50"))))
     (testing "an association that fits in one page does not ask for a total"
       (is (> (statements running :get (str "/writers/" (last writers) "/works?size=1"))
              (statements running :get (str "/writers/" (last writers) "/works?size=20"))))))))  
