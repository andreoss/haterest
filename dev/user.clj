(ns user
  (:require [hypermedia.api :as api]
            [hypermedia.config :as config]
            [hypermedia.main :as main]
            [hypermedia.server :as server]
            [hypermedia.store.memory :as memory]))

(defonce running (atom nil))

(def seed
  {:author {1 {:id 1 :name "Herbert"}}
   :book   {1 {:id 1 :title "Dune" :year 1965 :author-id 1}
            2 {:id 2 :title "Messiah" :year 1969 :author-id 1}}})

(defn halt []
  (swap! running (fn [current] (some-> current :stop (apply [])) nil)))

(defn go
  ([] (go {:schema "example.edn"}))
  ([{:keys [schema database]}]
   (halt)
   (let [started (if database
                   (main/start {:schema schema :database database :port 0 :migrate true})
                   (server/start (api/handler (config/api schema) (memory/store seed)) {:port 0}))]
     (reset! running started)
     (:port started))))
