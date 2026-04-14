(ns hypermedia.health
  (:require [hypermedia.store :as store]))

(defn report [store]
  (let [reachable (try (boolean (store/probe store)) (catch Exception _ false))
        state     (if reachable "up" "down")]
    {:status state :store state}))

(defn up? [report]
  (= "up" (:status report)))
