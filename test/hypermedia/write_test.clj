(ns hypermedia.write-test
  (:require [clojure.test :refer [deftest is]]
            [hypermedia.api :as api]
            [hypermedia.store.memory :as memory]
            [jsonista.core :as json]))

(def config
  {:resources
   {:author {:fields {:id   {:type :uuid :identity true :generated true}
                      :name {:type :string :required true}}
             :relations {:books {:kind :has-many :target :book :via :author-id}}}
    :book   {:fields    {:id        {:type :uuid :identity true :generated true}
                         :title     {:type :string :required true}
                         :year      {:type :long}
                         :author-id {:type :uuid}}
             :relations {:author {:kind :belongs-to :target :author :via :author-id}}}}})

(api/defapi demo config)

(defn- subject [] (api/handler demo (memory/store {:author {} :book {}})))

(defn- call
  ([handler method path] (call handler method path nil))
  ([handler method path body]
   (let [response (handler (cond-> {:request-method method :uri path}
                             body (assoc :body (json/write-value-as-string body)
                                         :headers {"content-type" "application/json"})))]
     (assoc response :body (some-> (:body response) (json/read-value json/keyword-keys-object-mapper))))))

(deftest creates-and-answers-with-a-location
  (let [handler  (subject)
        response (call handler :post "/books" {:title "Dune" :year 1965})]
    (is (= 201 (:status response)))
    (is (= "Dune" (get-in response [:body :title])))
    (let [location (get-in response [:headers "Location"])]
      (is (= location (get-in response [:body :_links :self :href])))
      (is (= "Dune" (get-in (call handler :get location) [:body :title]))))))

(deftest refuses-a-submission-that-does-not-fit
  (let [response (call (subject) :post "/books" {:year 1965})]
    (is (= 422 (:status response)))
    (is (= "application/problem+json;charset=utf-8" (get-in response [:headers "Content-Type"])))
    (is (= "title" (get-in response [:body :errors 0 :field])))))

(deftest refuses-an-unknown-field
  (is (= 422 (:status (call (subject) :post "/books" {:title "Dune" :sneaky 1})))))

(deftest refuses-a-body-it-cannot-read
  (let [handler (subject)]
    (is (= 415 (:status (handler {:request-method :post :uri "/books" :body "{}"}))))
    (is (= 400 (:status (call handler :post "/books" "nonsense"))))))

(deftest links-a-relation-given-as-a-link
  (let [handler (subject)
        author  (call handler :post "/authors" {:name "Herbert"})
        href    (get-in author [:headers "Location"])
        book    (call handler :post "/books" {:title "Dune" :author href})]
    (is (= 201 (:status book)))
    (is (= "Herbert" (get-in (call handler :get (get-in book [:body :_links :rel:author :href]))
                             [:body :name])))))

(deftest replaces-and-creates-with-put
  (let [handler (subject)
        created (call handler :post "/books" {:title "Dune" :year 1965})
        self    (get-in created [:headers "Location"])
        put     (call handler :put self {:title "Dune Messiah"})]
    (is (= 200 (:status put)))
    (is (= "Dune Messiah" (get-in put [:body :title])))
    (is (nil? (get-in put [:body :year])))
    (is (= 201 (:status (call handler :put "/books/0-0-0-0-9" {:title "New"}))))))

(deftest refuses-a-put-whose-body-addresses-another-resource
  (let [handler (subject)
        created (call handler :post "/books" {:title "Dune"})]
    (is (= 409 (:status (call handler :put (get-in created [:headers "Location"])
                              {:id "0-0-0-0-9" :title "Dune"}))))))

(deftest amends-part-of-a-resource
  (let [handler (subject)
        created (call handler :post "/books" {:title "Dune" :year 1965})
        self    (get-in created [:headers "Location"])
        patch   (call handler :patch self {:year 1966})]
    (is (= 200 (:status patch)))
    (is (= "Dune" (get-in patch [:body :title])))
    (is (= 1966 (get-in patch [:body :year])))
    (is (= 404 (:status (call handler :patch "/books/0-0-0-0-9" {:year 1}))))))

(deftest removes-a-resource
  (let [handler (subject)
        created (call handler :post "/books" {:title "Dune"})
        self    (get-in created [:headers "Location"])]
    (is (= 204 (:status (call handler :delete self))))
    (is (= 404 (:status (call handler :get self))))
    (is (= 404 (:status (call handler :delete self))))))

(deftest refuses-a-method-the-resource-does-not-offer
  (is (= 405 (:status (call (subject) :delete "/books")))))

(deftest refuses-an-identity-it-cannot-read
  (is (= 404 (:status (call (subject) :get "/books/not-a-uuid")))))
