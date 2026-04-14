(ns hypermedia.health-test
  (:require [clojure.test :refer [deftest is]]
            [hypermedia.health :as health]
            [hypermedia.store :as store]))

(defrecord Down []
  store/Store
  (fetch [_ _ _] nil)
  (query [_ _ _] [])
  (total [_ _ _] 0)
  (probe [_] (throw (ex-info "no route to host" {}))))

(defrecord Up []
  store/Store
  (fetch [_ _ _] nil)
  (query [_ _ _] [])
  (total [_ _ _] 0)
  (probe [_] true))

(deftest reports-a-reachable-store
  (is (= {:status "up" :store "up"} (health/report (->Up)))))

(deftest reports-an-unreachable-store
  (let [report (health/report (->Down))]
    (is (= "down" (:status report)))
    (is (= "down" (:store report)))))

(deftest a-false-probe-is-down
  (is (= "down" (:status (health/report (reify store/Store
                                          (fetch [_ _ _] nil)
                                          (query [_ _ _] [])
                                          (total [_ _ _] 0)
                                          (probe [_] false)))))))
