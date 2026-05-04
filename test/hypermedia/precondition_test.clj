(ns hypermedia.precondition-test
  (:require [clojure.test :refer [deftest is]]
            [hypermedia.api :as api]
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

(defn- subject []
  (api/handler demo (memory/store {:author {1 {:id 1 :name "Herbert"} 2 {:id 2 :name "Austen"}}
                                   :book   {1 {:id 1 :title "Dune" :author-id 1}}})))

(defn- call
  ([handler method path] (call handler method path {}))
  ([handler method path {:keys [body headers]}]
   (let [response (handler (cond-> {:request-method method :uri path :headers (or headers {})}
                             body (assoc :body body)))]
     (assoc response :body (some-> (:body response) (json/read-value json/keyword-keys-object-mapper))))))

(defn- tag-of [handler path]
  (get-in (call handler :get path) [:headers "ETag"]))

(deftest a-read-carries-a-tag
  (let [handler (subject)]
    (is (re-matches #"\".+\"" (tag-of handler "/books/1")))
    (is (= (tag-of handler "/books/1") (tag-of handler "/books/1")))))

(deftest an-unchanged-resource-is-not-sent-again
  (let [handler  (subject)
        tag      (tag-of handler "/books/1")
        response (call handler :get "/books/1" {:headers {"if-none-match" tag}})]
    (is (= 304 (:status response)))
    (is (nil? (:body response)))
    (is (= tag (get-in response [:headers "ETag"])))))

(deftest a-changed-resource-is-sent-again
  (let [handler (subject)
        tag     (tag-of handler "/books/1")]
    (call handler :patch "/books/1" {:body (json/write-value-as-string {:title "Messiah"})
                                     :headers {"content-type" "application/json"}})
    (is (= 200 (:status (call handler :get "/books/1" {:headers {"if-none-match" tag}}))))))

(deftest a-stale-write-is-refused
  (let [handler (subject)
        stale   (tag-of handler "/books/1")]
    (call handler :patch "/books/1" {:body    (json/write-value-as-string {:title "Messiah"})
                                     :headers {"content-type" "application/json"}})
    (doseq [[method body] [[:put (json/write-value-as-string {:title "Other"})]
                           [:patch (json/write-value-as-string {:title "Other"})]]]
      (let [response (call handler method "/books/1"
                           {:body body :headers {"content-type" "application/json" "if-match" stale}})]
        (is (= 412 (:status response)) (str method))))
    (is (= 412 (:status (call handler :delete "/books/1" {:headers {"if-match" stale}}))))))

(deftest a-current-write-is-accepted
  (let [handler (subject)
        current (tag-of handler "/books/1")
        response (call handler :patch "/books/1"
                       {:body    (json/write-value-as-string {:title "Messiah"})
                        :headers {"content-type" "application/json" "if-match" current}})]
    (is (= 200 (:status response)))
    (is (not= current (get-in response [:headers "ETag"])))))

(deftest a-stale-association-write-is-refused
  (let [handler (subject)
        stale   (tag-of handler "/books/1")]
    (call handler :patch "/books/1" {:body    (json/write-value-as-string {:title "Messiah"})
                                     :headers {"content-type" "application/json"}})
    (is (= 412 (:status (call handler :put "/books/1/author"
                              {:body "/authors/2" :headers {"content-type" "text/uri-list"
                                                            "if-match" stale}}))))))

(deftest a-write-without-a-precondition-is-allowed
  (is (= 200 (:status (call (subject) :patch "/books/1"
                            {:body    (json/write-value-as-string {:title "Messiah"})
                             :headers {"content-type" "application/json"}})))))

(deftest a-document-states-what-may-be-done-to-it
  (let [handler   (subject)
        item      (call handler :get "/books/1" {:headers {"accept" "application/prs.hal-forms+json"}})
        templates (get-in item [:body :_templates])]
    (is (= "application/prs.hal-forms+json;charset=utf-8" (get-in item [:headers "Content-Type"])))
    (is (= "PUT" (get-in templates [:default :method])))
    (is (= "PATCH" (get-in templates [:patch :method])))
    (is (= "DELETE" (get-in templates [:delete :method])))
    (is (= [{:name "title" :type "text" :required true}
            {:name "author" :type "url" :required false}]
           (get-in templates [:default :properties])))))

(deftest a-collection-states-how-to-add-to-it
  (let [collection (call (subject) :get "/books" {:headers {"accept" "application/prs.hal-forms+json"}})]
    (is (= "POST" (get-in collection [:body :_templates :default :method])))
    (is (= "/books" (get-in collection [:body :_templates :default :target])))))

(deftest plain-hal-carries-no-templates
  (is (nil? (get-in (call (subject) :get "/books/1") [:body :_templates])))
  (is (= "application/hal+json;charset=utf-8"
         (get-in (call (subject) :get "/books/1") [:headers "Content-Type"]))))
