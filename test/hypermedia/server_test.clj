(ns hypermedia.server-test
  (:require [clojure.test :refer [deftest is]]
            [hypermedia.api :as api]
            [hypermedia.api-test :as fixture]
            [hypermedia.server :as server]
            [jsonista.core :as json])
  (:import (java.net HttpURLConnection URI)))

(defn- fetch [port path]
  (let [connection ^HttpURLConnection (.openConnection (.toURL (URI. (str "http://127.0.0.1:" port path))))]
    (doto connection
      (.setRequestProperty "Connection" "close")
      (.setRequestProperty "Accept" "application/hal+json")
      (.setConnectTimeout 2000)
      (.setReadTimeout 2000))
    (let [status (.getResponseCode connection)
          body   (slurp (.getInputStream connection))]
      (.disconnect connection)
      {:status status :body (json/read-value body json/keyword-keys-object-mapper)})))

(deftest serves-over-http
  (let [handler (api/handler fixture/demo fixture/store)
        running (server/start handler {:port 0})]
    (try
      (is (pos? (:port running)))
      (let [response (fetch (:port running) "/")]
        (is (= 200 (:status response)))
        (is (= "/books" (get-in response [:body :_links :rel:books :href]))))
      (let [response (fetch (:port running) "/books/1")]
        (is (= "Dune" (get-in response [:body :title]))))
      (finally ((:stop running))))))
