(ns hypermedia.discovery-test
  (:require [clojure.test :refer [deftest is]]
            [hypermedia.api :as api]
            [hypermedia.schema]
            [hypermedia.store.memory :as memory]
            [jsonista.core :as json]))

(def config
  {:resources
   {:author {:fields    {:id {:type :long :identity true} :name {:type :string :required true}}
             :relations {:books {:kind :has-many :target :book :via :author-id}}}
    :book   {:fields    {:id        {:type :long :identity true}
                         :title     {:type :string :required true}
                         :author-id {:type :long}}
             :relations {:author {:kind :belongs-to :target :author :via :author-id}}}}})

(api/defapi demo config)

(def handler
  (api/handler demo (memory/store {:author {1 {:id 1 :name "Herbert"}}
                                   :book   {1 {:id 1 :title "Dune" :author-id 1}}})))

(defn- call
  ([method path] (call method path nil))
  ([method path accept]
   (let [response (handler (cond-> {:request-method method :uri path}
                             accept (assoc :headers {"accept" accept})))]
     (assoc response :body (some-> (:body response) (json/read-value json/keyword-keys-object-mapper))))))

(deftest the-root-declares-its-curie
  (let [body (:body (call :get "/"))]
    (is (= [{:name "rel" :href "/profile/{rel}" :templated true}] (:curies (:_links body))))
    (is (= "/profile" (get-in body [:_links :profile :href])))))

(deftest a-schema-may-decline-the-curie
  (let [plain (api/handler (api/build (hypermedia.schema/parse (assoc config :curie false)))
                           (memory/store {}))
        body  (json/read-value (:body (plain {:request-method :get :uri "/"}))
                               json/keyword-keys-object-mapper)]
    (is (nil? (:curies (:_links body))))
    (is (= "/books" (get-in body [:_links :books :href])))))

(deftest the-profile-lists-every-resource
  (let [body (:body (call :get "/profile"))]
    (is (= "/profile" (get-in body [:_links :self :href])))
    (is (= "/profile/books" (get-in body [:_links :rel:books :href])))
    (is (= "/profile/authors" (get-in body [:_links :rel:authors :href])))))

(deftest a-resource-describes-itself
  (let [response (call :get "/profile/books" "application/alps+json")
        alps     (get-in response [:body :alps])
        first-of (first (:descriptor alps))]
    (is (= "application/alps+json;charset=utf-8" (get-in response [:headers "Content-Type"])))
    (is (= "1.0" (:version alps)))
    (is (= "book-representation" (:id first-of)))
    (is (= ["title" "author"] (map :name (:descriptor first-of))))
    (is (= #{"get-books" "create-books" "get-book" "update-book" "patch-book" "delete-book"}
           (set (keep :id (rest (:descriptor alps))))))
    (is (= ["page" "size" "sort"]
           (map :name (:descriptor (first (filter #(= "get-books" (:id %)) (:descriptor alps)))))))))

(deftest a-document-points-at-its-profile
  (is (= "/profile/books" (get-in (call :get "/books/1") [:body :_links :profile :href])))
  (is (= "/profile/books" (get-in (call :get "/books") [:body :_links :profile :href]))))

(deftest it-refuses-what-it-cannot-produce
  (is (= 406 (:status (call :get "/books" "text/csv"))))
  (is (= 406 (:status (call :get "/books/1" "text/csv"))))
  (is (= 406 (:status (call :get "/profile" "text/csv"))))
  (is (= 406 (:status (call :get "/profile/books" "text/csv"))))
  (is (= 200 (:status (call :get "/books" "application/hal+json"))))
  (is (= 200 (:status (call :get "/books" "*/*")))))

(deftest it-states-what-a-resource-allows
  (is (= "GET, POST, OPTIONS" (get-in (call :options "/books") [:headers "Allow"])))
  (is (= "GET, PUT, PATCH, DELETE, OPTIONS" (get-in (call :options "/books/1") [:headers "Allow"])))
  (is (= "GET, PUT, DELETE, OPTIONS" (get-in (call :options "/books/1/author") [:headers "Allow"])))
  (is (= "GET, POST, PUT, OPTIONS" (get-in (call :options "/authors/1/books") [:headers "Allow"])))
  (is (= "GET, OPTIONS" (get-in (call :options "/profile/books") [:headers "Allow"]))))

(deftest a-refused-method-states-what-is-allowed
  (let [response (call :delete "/books")]
    (is (= 405 (:status response)))
    (is (= "GET, POST, OPTIONS" (get-in response [:headers "Allow"])))))

(defn- hrefs [node]
  (cond
    (map? node) (concat (for [[_ link] (:_links node)
                              :when (and (map? link) (not (:templated link)))]
                          (:href link))
                        (mapcat hrefs (vals (dissoc node :_links))))
    (sequential? node) (mapcat hrefs node)
    :else nil))

(deftest every-resource-is-reachable-from-the-root
  (loop [pending ["/"] seen #{} guard 0]
    (if (or (empty? pending) (> guard 50))
      (do
        (is (< guard 50) "the crawl did not settle")
        (is (every? seen ["/" "/books" "/authors" "/books/1" "/authors/1"
                          "/books/1/author" "/authors/1/books"
                          "/profile" "/profile/books" "/profile/authors" "/health"])))
      (let [href     (first pending)
            bare     (first (clojure.string/split href #"\?"))
            response (handler {:request-method :get
                               :uri            bare
                               :query-string   (second (clojure.string/split href #"\?"))})
            body     (some-> (:body response) (json/read-value json/keyword-keys-object-mapper))]
        (is (= 200 (:status response)) href)
        (recur (into (rest pending)
                     (remove #(contains? (conj seen bare) (first (clojure.string/split % #"\?")))
                             (hrefs body)))
               (conj seen bare)
               (inc guard))))))
