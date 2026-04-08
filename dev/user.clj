(ns user
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [hypermedia.api :as api]
            [hypermedia.route :as route]
            [hypermedia.schema :as schema]
            [hypermedia.server :as server]
            [hypermedia.store.memory :as memory]))

(defonce running (atom nil))

(def seed
  {:author {1 {:id 1 :name "Herbert"}}
   :book   {1 {:id 1 :title "Dune" :year 1965 :author-id 1}
            2 {:id 2 :title "Messiah" :year 1969 :author-id 1}}})

(defn go []
  (let [model (schema/parse (edn/read-string (slurp (io/resource "example.edn"))))
        api   {:model model :routes (route/routes model)}]
    (swap! running (fn [current]
                     (some-> current :stop (apply []))
                     (server/start (api/handler api (memory/store seed)) {:port 0})))
    (:port @running)))

(defn halt []
  (swap! running (fn [current] (some-> current :stop (apply [])) nil)))
