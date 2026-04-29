(ns hypermedia.association-test
  (:require [clojure.test :refer [deftest is]]
            [hypermedia.api :as api]
            [hypermedia.store.counting :as counting]
            [hypermedia.store.memory :as memory]
            [jsonista.core :as json]))

(def config
  {:resources
   {:author {:fields    {:id {:type :long :identity true} :name {:type :string :required true}}
             :relations {:books {:kind :has-many :target :book :via :author-id :embed true}}}
    :book   {:fields    {:id        {:type :long :identity true}
                         :title     {:type :string :required true}
                         :author-id {:type :long}}
             :relations {:author {:kind :belongs-to :target :author :via :author-id :embed true}}}}})

(api/defapi demo config)

(defn- rows []
  {:author {1 {:id 1 :name "Herbert"} 2 {:id 2 :name "Austen"}}
   :book   {1 {:id 1 :title "Dune" :author-id 1}
            2 {:id 2 :title "Messiah" :author-id 1}
            3 {:id 3 :title "Emma" :author-id 2}}})

(defn- subject []
  (let [store   (counting/counting (memory/store (rows)))
        handler (api/handler demo store)]
    {:store store
     :call  (fn call
              ([method path] (call method path nil nil))
              ([method path body content-type]
               (let [[uri query] (clojure.string/split path #"\?" 2)
                     response (handler (cond-> {:request-method method :uri uri :query-string query}
                                         body (assoc :body body
                                                     :headers {"content-type" content-type})))]
                 (assoc response :body (some-> (:body response)
                                               (json/read-value json/keyword-keys-object-mapper))))))}))

(deftest an-item-embeds-what-the-schema-says-to-embed
  (let [{:keys [call]} (subject)
        body (:body (call :get "/books/1"))]
    (is (= "Herbert" (get-in body [:_embedded :author :name])))
    (is (= "/authors/1" (get-in body [:_embedded :author :_links :self :href])))))

(deftest embedding-stops-at-one-level
  (let [{:keys [call]} (subject)
        author (get-in (call :get "/books/1") [:body :_embedded :author])]
    (is (nil? (:_embedded author)))
    (is (= "/authors/1/books" (get-in author [:_links :rel:books :href])))))

(deftest a-collection-embeds-without-a-query-per-row
  (let [{:keys [call store]} (subject)
        body (:body (call :get "/books"))]
    (is (= ["Herbert" "Herbert" "Austen"]
           (map #(get-in % [:_embedded :author :name]) (get-in body [:_embedded :books]))))
    (is (= 2 (:query (counting/tally store))))))

(deftest an-author-embeds-the-books-it-owns
  (let [{:keys [call]} (subject)
        body (:body (call :get "/authors/1"))]
    (is (= #{"Dune" "Messiah"} (set (map :title (get-in body [:_embedded :books])))))))

(deftest a-reference-moves-a-resource
  (let [{:keys [call]} (subject)]
    (is (= 204 (:status (call :put "/books/1/author" "/authors/2" "text/uri-list"))))
    (is (= "Austen" (get-in (call :get "/books/1/author") [:body :name])))))

(deftest a-reference-is-refused-when-it-does-not-fit
  (let [{:keys [call]} (subject)]
    (is (= 415 (:status (call :put "/books/1/author" "/authors/2" "text/plain"))))
    (is (= 422 (:status (call :put "/books/1/author" "/books/2" "text/uri-list"))))
    (is (= 422 (:status (call :put "/books/1/author" "/authors/9" "text/uri-list"))))
    (is (= 422 (:status (call :put "/books/1/author" "/authors/1\n/authors/2" "text/uri-list"))))
    (is (= 404 (:status (call :put "/books/9/author" "/authors/1" "text/uri-list"))))))

(deftest a-reference-is-removed
  (let [{:keys [call]} (subject)]
    (is (= 204 (:status (call :delete "/books/1/author"))))
    (is (= 404 (:status (call :get "/books/1/author"))))))

(deftest a-set-of-references-is-replaced
  (let [{:keys [call]} (subject)]
    (is (= 204 (:status (call :put "/authors/1/books" "/books/3" "text/uri-list"))))
    (is (= ["Emma"] (map :title (get-in (call :get "/authors/1/books") [:body :_embedded :books]))))
    (is (empty? (get-in (call :get "/authors/2/books") [:body :_embedded :books])))))

(deftest a-reference-is-added-to-a-set
  (let [{:keys [call]} (subject)]
    (is (= 204 (:status (call :post "/authors/1/books" "/books/3" "text/uri-list"))))
    (is (= 3 (count (get-in (call :get "/authors/1/books") [:body :_embedded :books]))))))

(deftest one-member-is-taken-out-of-a-set
  (let [{:keys [call]} (subject)]
    (is (= 204 (:status (call :delete "/authors/1/books/2"))))
    (is (= ["Dune"] (map :title (get-in (call :get "/authors/1/books") [:body :_embedded :books]))))
    (is (= 404 (:status (call :delete "/authors/1/books/3"))))
    (is (= 404 (:status (call :delete "/authors/1/books/9"))))))

(deftest a-has-many-relation-is-not-removed-as-a-whole
  (is (= 405 (:status ((:call (subject)) :delete "/authors/1/books")))))
