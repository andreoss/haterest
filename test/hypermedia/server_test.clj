(ns hypermedia.server-test
  (:require [clojure.test :refer [deftest is testing]]
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

(deftest a-request-in-flight-finishes-before-the-server-stops
  (let [entered  (java.util.concurrent.CountDownLatch. 1)
        answered (atom nil)
        slow     (fn [_]
                   (.countDown entered)
                   (Thread/sleep 700)
                   {:status 200 :headers {"Content-Type" "text/plain"} :body "finished"})
        running  (server/start slow {:port 0 :drain 5000})
        caller   (future (client/request (:port running) :get "/slow"))]
    (.await entered)
    (let [began   (System/currentTimeMillis)
          _       ((:stop running))
          stopped (- (System/currentTimeMillis) began)]
      (reset! answered @caller)
      (testing "the caller is answered rather than cut off"
        (is (= 200 (:status @answered)))
        (is (= "finished" (:raw @answered))))
      (testing "and stopping waited for it"
        (is (<= 300 stopped) (str "stop returned after " stopped "ms"))))))

(deftest the-server-refuses-work-once-it-has-stopped
  (let [running (server/start (fn [_] {:status 200 :headers {} :body "ok"}) {:port 0})
        port    (:port running)]
    (is (= 200 (:status (client/request port :get "/"))))
    ((:stop running))
    (is (thrown? Exception (client/request port :get "/")))))
