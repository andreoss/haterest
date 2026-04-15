(ns hypermedia.main-test
  (:require [clojure.test :refer [deftest is]]
            [hypermedia.main :as main]
            [jsonista.core :as json])
  (:import (java.net HttpURLConnection URI)))

(defn- fetch [port path]
  (let [connection ^HttpURLConnection (.openConnection (.toURL (URI. (str "http://127.0.0.1:" port path))))]
    (doto connection
      (.setRequestProperty "Connection" "close")
      (.setConnectTimeout 2000)
      (.setReadTimeout 2000))
    (let [status (.getResponseCode connection)
          stream (if (< status 400) (.getInputStream connection) (.getErrorStream connection))
          body   (json/read-value (slurp stream) json/keyword-keys-object-mapper)]
      (.disconnect connection)
      {:status status :body body})))

(deftest parses-options
  (let [options (main/options ["-s" "example.edn" "-d" "jdbc:h2:mem:x" "-p" "0"])]
    (is (= "example.edn" (:schema options)))
    (is (= "jdbc:h2:mem:x" (:database options)))
    (is (= 0 (:port options)))))

(deftest requires-a-schema-and-a-database
  (is (seq (main/problems (main/options [])))))

(deftest boots-from-a-schema-and-a-url
  (let [running (main/start {:schema   "example.edn"
                             :database (str "jdbc:h2:mem:" (gensym "main") ";DB_CLOSE_DELAY=-1")
                             :port     0
                             :migrate  true})]
    (try
      (is (pos? (:port running)))
      (is (= "/books" (get-in (fetch (:port running) "/") [:body :_links :books :href])))
      (is (= 200 (:status (fetch (:port running) "/health"))))
      (is (= "up" (get-in (fetch (:port running) "/health") [:body :status])))
      (is (= [] (get-in (fetch (:port running) "/books") [:body :_embedded :books])))
      (finally ((:stop running))))))

(deftest reports-an-unreachable-store-as-unhealthy
  (let [running (main/start {:schema   "example.edn"
                             :database (str "jdbc:h2:mem:" (gensym "main") ";DB_CLOSE_DELAY=-1")
                             :port     0
                             :migrate  true})]
    (try
      (main/detach running)
      (is (= 503 (:status (fetch (:port running) "/health"))))
      (finally ((:stop running))))))
