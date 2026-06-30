(ns hypermedia.many-test
  (:require [clojure.string :as string]
            [clojure.test :refer [deftest is]]
            [hypermedia.api :as api]
            [hypermedia.config :as config]
            [hypermedia.conformance :refer [defconformance]]
            [hypermedia.schema :as schema]
            [hypermedia.store.counting :as counting]
            [hypermedia.store.memory :as memory]
            [jsonista.core :as json]))

(def config
  {:resources
   {:author {:fields    {:id {:type :long :identity true} :name {:type :string :required true}}
             :relations {:books {:kind :many-to-many :target :book :through :authorship
                                 :via :author-id :target-via :book-id :embed true}}}
    :book   {:fields    {:id {:type :long :identity true} :title {:type :string :required true}}
             :relations {:authors {:kind :many-to-many :target :author :through :authorship
                                   :via :book-id :target-via :author-id}}}}})

(def model (schema/parse config))

(api/defapi demo config)

(defn- rows []
  {:author     {1 {:id 1 :name "Herbert"} 2 {:id 2 :name "Anderson"} 3 {:id 3 :name "Austen"}}
   :book       {1 {:id 1 :title "Dune"} 2 {:id 2 :title "Sandworms"} 3 {:id 3 :title "Emma"}}
   :authorship #{{:author_id 1 :book_id 1} {:author_id 1 :book_id 2}
                 {:author_id 2 :book_id 2} {:author_id 3 :book_id 3}}})

(defn- subject []
  (let [store (counting/counting (memory/store (rows)))]
    {:store store
     :call  (let [handler (api/handler demo store)]
              (fn call
                ([method path] (call method path nil nil))
                ([method path body content-type]
                 (let [[uri query] (string/split path #"\?" 2)
                       response (handler (cond-> {:request-method method :uri uri :query-string query}
                                           body (assoc :body body
                                                       :headers {"content-type" content-type})))]
                   (assoc response :body (some-> (:body response)
                                                 (json/read-value json/keyword-keys-object-mapper)))))))}))

(deftest the-join-is-declared-once-and-mirrored
  (is (= {:table :authorship :via-column :author_id :target-via-column :book_id}
         (get-in model [:resources :author :relations :books :join])))
  (is (= {:table :authorship :via-column :book_id :target-via-column :author_id}
         (get-in model [:resources :book :relations :authors :join]))))

(deftest a-join-must-be-fully-declared
  (is (thrown? clojure.lang.ExceptionInfo
               (schema/parse (update-in config [:resources :author :relations :books] dissoc :through))))
  (is (thrown? clojure.lang.ExceptionInfo
               (schema/parse (update-in config [:resources :author :relations :books] dissoc :target-via))))
  (is (thrown? clojure.lang.ExceptionInfo
               (schema/parse (assoc-in config [:resources :author :relations :books :target-via] :author-id))))
  (is (thrown? clojure.lang.ExceptionInfo
               (schema/parse (assoc-in config [:resources :author :relations :books :through] :books))))
  (is (thrown? clojure.lang.ExceptionInfo
               (schema/parse (assoc-in config [:resources :book :relations :authors :kind] :has-many)))))

(deftest both-sides-are-addressable
  (let [{:keys [call]} (subject)]
    (is (= ["Dune" "Sandworms"]
           (sort (map :title (get-in (call :get "/authors/1/books") [:body :_embedded :books])))))
    (is (= ["Anderson" "Herbert"]
           (sort (map :name (get-in (call :get "/books/2/authors") [:body :_embedded :authors])))))
    (is (= 2 (get-in (call :get "/authors/1/books") [:body :page :totalElements])))))

(deftest a-traversal-is-a-slice
  (let [{:keys [call]} (subject)
        body (:body (call :get "/authors/1/books?size=1&sort=title,asc"))]
    (is (= ["Dune"] (map :title (get-in body [:_embedded :books]))))
    (is (string/starts-with? (get-in body [:_links :next :href])
                             "/authors/1/books?page=1&size=1&sort=title%2Casc"))
    (is (= ["Sandworms"]
           (map :title (get-in (call :get "/authors/1/books?page=1&size=1&sort=title,asc")
                               [:body :_embedded :books]))))))

(deftest each-side-counts-its-own-links
  (let [{:keys [call]} (subject)]
    (is (= 1 (get-in (call :get "/books/1/authors") [:body :page :totalElements])))
    (is (= 2 (get-in (call :get "/books/2/authors") [:body :page :totalElements])))
    (is (= 2 (get-in (call :get "/authors/1/books") [:body :page :totalElements])))))

(deftest a-page-embeds-through-the-join-at-a-fixed-cost
  (let [{:keys [call store]} (subject)
        body (:body (call :get "/authors"))]
    (is (= [["Dune" "Sandworms"] ["Sandworms"] ["Emma"]]
           (map #(sort (map :title (get-in % [:_embedded :books])))
                (get-in body [:_embedded :authors]))))
    (is (= 3 (:query (counting/tally store))))
    (counting/reset-tally store)
    (call :get "/authors?size=1")
    (is (= 3 (:query (counting/tally store))))))

(deftest a-link-is-added-and-removed
  (let [{:keys [call]} (subject)]
    (is (= 204 (:status (call :post "/authors/3/books" "/books/1" "text/uri-list"))))
    (is (= ["Dune" "Emma"]
           (sort (map :title (get-in (call :get "/authors/3/books") [:body :_embedded :books])))))
    (is (= 204 (:status (call :delete "/authors/3/books/1"))))
    (is (= ["Emma"] (map :title (get-in (call :get "/authors/3/books") [:body :_embedded :books]))))
    (is (= 404 (:status (call :delete "/authors/3/books/1"))))))

(deftest adding-a-link-twice-does-not-double-it
  (let [{:keys [call]} (subject)]
    (call :post "/authors/1/books" "/books/1" "text/uri-list")
    (is (= 2 (get-in (call :get "/authors/1/books") [:body :page :totalElements])))))

(deftest a-set-of-links-is-replaced
  (let [{:keys [call]} (subject)]
    (is (= 204 (:status (call :put "/authors/1/books" "/books/3" "text/uri-list"))))
    (is (= ["Emma"] (map :title (get-in (call :get "/authors/1/books") [:body :_embedded :books]))))
    (is (= ["Anderson"] (map :name (get-in (call :get "/books/2/authors") [:body :_embedded :authors]))))))

(deftest a-reference-outside-the-target-is-refused
  (let [{:keys [call]} (subject)]
    (is (= 422 (:status (call :put "/authors/1/books" "/authors/2" "text/uri-list"))))
    (is (= 422 (:status (call :put "/authors/1/books" "/books/9" "text/uri-list"))))
    (is (= 415 (:status (call :put "/authors/1/books" "/books/3" "text/plain"))))
    (is (= 404 (:status (call :put "/authors/9/books" "/books/3" "text/uri-list"))))))

(deftest the-relation-states-what-it-allows
  (let [{:keys [call]} (subject)]
    (is (= "GET, POST, PUT, OPTIONS" (get-in (call :options "/authors/1/books") [:headers "Allow"])))
    (is (= 405 (:status (call :delete "/authors/1/books"))))))

(deftest the-profile-names-the-kind
  (let [{:keys [call]} (subject)
        response (call :get "/profile/authors")
        alps     (:alps (:body response))
        represents (first (:descriptor alps))]
    (is (= "many-to-many"
           (->> (:descriptor represents) (filter #(= "books" (:name %))) first :doc :value)))))

(def catalogue (config/api "catalogue.edn"))

(defconformance catalogue catalogue (memory/store {:author {} :book {} :authorship #{}}))

(deftest the-same-reference-twice-is-one-link
  (let [{:keys [call]} (subject)]
    (is (= 204 (:status (call :post "/authors/3/books" "/books/1\n/books/1" "text/uri-list"))))
    (is (= 2 (get-in (call :get "/authors/3/books") [:body :page :totalElements])))))
