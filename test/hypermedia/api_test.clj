(ns hypermedia.api-test
  (:require [clojure.test :refer [deftest is]]
            [hypermedia.api :as api]
            [hypermedia.store.memory :as memory]
            [jsonista.core :as json]))

(def config
  {:resources
   {:author {:fields    {:id {:type :long :identity true} :name {:type :string}}
             :relations {:books {:kind :has-many :target :book :via :author-id}}}
    :book   {:fields    {:id        {:type :long :identity true}
                         :title     {:type :string}
                         :author-id {:type :long}}
             :relations {:author {:kind :belongs-to :target :author :via :author-id}}}}})

(api/defapi demo config)

(def store
  (memory/store {:author {1 {:id 1 :name "Herbert"}}
                 :book   {1 {:id 1 :title "Dune" :author-id 1}
                          2 {:id 2 :title "Messiah" :author-id 1}}}))

(def handler (api/handler demo store))

(defn- GET [path]
  (let [response (handler {:request-method :get :uri path})]
    (assoc response :body (some-> (:body response) (json/read-value json/keyword-keys-object-mapper)))))

(deftest expands-the-schema-at-compile-time
  (is (= [:author :book] (:order (:model demo))))
  (is (vector? (:routes demo))))

(deftest rejects-an-invalid-schema-at-expansion
  (is (thrown? Exception
               (macroexpand '(hypermedia.api/defapi bad {:resources {:a {:fields {:x {:type :string}}}}})))))

(deftest serves-hal
  (is (= "application/hal+json;charset=utf-8" (get-in (GET "/") [:headers "Content-Type"]))))

(deftest root-links-every-collection
  (let [body (:body (GET "/"))]
    (is (= "/" (get-in body [:_links :self :href])))
    (is (= "/authors" (get-in body [:_links :authors :href])))
    (is (= "/books" (get-in body [:_links :books :href])))))

(deftest collection-embeds-items
  (let [body (:body (GET "/books"))]
    (is (= "/books?page=0&size=20" (get-in body [:_links :self :href])))
    (is (= #{"Dune" "Messiah"} (set (map :title (get-in body [:_embedded :books])))))
    (is (= #{"/books/1" "/books/2"}
           (set (map #(get-in % [:_links :self :href]) (get-in body [:_embedded :books])))))))

(deftest item-carries-self-and-relation-links
  (let [body (:body (GET "/books/1"))]
    (is (= "Dune" (:title body)))
    (is (= "/books/1" (get-in body [:_links :self :href])))
    (is (= "/books/1/author" (get-in body [:_links :author :href])))))

(deftest item-hides-identity-and-foreign-keys
  (let [body (:body (GET "/books/1"))]
    (is (not (contains? body :id)))
    (is (not (contains? body :author-id)))))

(deftest follows-a-belongs-to-association
  (let [body (:body (GET "/books/1/author"))]
    (is (= "Herbert" (:name body)))
    (is (= "/authors/1" (get-in body [:_links :self :href])))))

(deftest follows-a-has-many-association
  (let [body (:body (GET "/authors/1/books"))]
    (is (= "/authors/1/books?page=0&size=20" (get-in body [:_links :self :href])))
    (is (= 2 (count (get-in body [:_embedded :books]))))))

(deftest reports-a-missing-item
  (is (= 404 (:status (GET "/books/99")))))

(deftest reports-an-unknown-path
  (is (= 404 (:status (GET "/nowhere")))))

(deftest reports-an-unusable-identity
  (is (= 404 (:status (GET "/books/seven")))))
