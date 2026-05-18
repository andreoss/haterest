(ns hypermedia.server-test
  (:require [clojure.test :refer [deftest is]]
            [hypermedia.api :as api]
            [hypermedia.api-test :as fixture]
            [hypermedia.client :as client]
            [hypermedia.server :as server]))

(deftest serves-over-http
  (let [handler (api/handler fixture/demo fixture/store)
        running (server/start handler {:port 0})]
    (try
      (is (pos? (:port running)))
      (let [response (client/request (:port running) :get "/" :accept "application/hal+json")]
        (is (= 200 (:status response)))
        (is (= "/books" (get-in response [:body :_links :rel:books :href]))))
      (let [response (client/request (:port running) :get "/books/1")]
        (is (= "Dune" (get-in response [:body :title]))))
      (finally ((:stop running))))))

(deftest announces-the-port-it-was-given
  (let [running (server/start (fn [_] {:status 200 :headers {} :body "{}"}) {:port 0})]
    (try
      (is (not= 0 (:port running)))
      (is (= "127.0.0.1" (:host running)))
      (finally ((:stop running))))))
