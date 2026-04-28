(ns hypermedia.main-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hypermedia.main :as main]
            [jsonista.core :as json])
  (:import (java.net URI)
           (java.net.http HttpClient HttpClient$Version HttpRequest HttpRequest$BodyPublishers
                          HttpResponse$BodyHandlers)
           (java.time Duration)))

(def ^:private client
  (-> (HttpClient/newBuilder)
      (.version HttpClient$Version/HTTP_1_1)
      (.connectTimeout (Duration/ofSeconds 2))
      (.build)))

(defn- request
  ([port method path] (request port method path nil))
  ([port method path payload]
   (let [body     (if payload
                    (HttpRequest$BodyPublishers/ofString (json/write-value-as-string payload))
                    (HttpRequest$BodyPublishers/noBody))
         built    (cond-> (-> (HttpRequest/newBuilder (URI/create (str "http://127.0.0.1:" port path)))
                              (.timeout (Duration/ofSeconds 2))
                              (.header "Connection" "close")
                              (.method (str/upper-case (name method)) body))
                    payload (.header "Content-Type" "application/json"))
         response (.send client (.build built) (HttpResponse$BodyHandlers/ofString))
         text     (.body response)]
     {:status   (.statusCode response)
      :location (.orElse (.firstValue (.headers response) "location") nil)
      :body     (when (seq text) (json/read-value text json/keyword-keys-object-mapper))})))

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
