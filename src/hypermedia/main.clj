(ns hypermedia.main
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.tools.cli :as cli]
            [hypermedia.api :as api]
            [hypermedia.config :as config]
            [hypermedia.server :as server]
            [hypermedia.store.jdbc :as jdbc-store]))

(def specification
  [["-s" "--schema PATH" "schema file or resource"]
   ["-d" "--database URL" "jdbc url"]
   ["-p" "--port PORT" "port to bind, zero for any" :default 8080 :parse-fn parse-long]
   ["-h" "--host HOST" "address to bind" :default "127.0.0.1"]
   [nil "--migrate" "derive and apply the schema before serving" :default false]
   [nil "--help"]])

(defn options [arguments]
  (let [{:keys [options errors]} (cli/parse-opts arguments specification)]
    (assoc options :errors errors)))

(defn problems [options]
  (cond-> (vec (:errors options))
    (str/blank? (:schema options))   (conj "a schema is required")
    (str/blank? (:database options)) (conj "a database url is required")))

(defn start [{:keys [schema database port host migrate]}]
  (let [api   (config/api schema)
        store (jdbc-store/open {:url database :model (:model api) :migrate? (boolean migrate)})
        running (server/start (api/handler api store) {:port (or port 8080)
                                                       :host (or host "127.0.0.1")})]
    (assoc running
           :store store
           :stop (fn []
                   ((:stop running))
                   (jdbc-store/close store)))))

(defn detach [running]
  (jdbc-store/close (:store running)))

(defn -main [& arguments]
  (let [options (options arguments)]
    (cond
      (:help options) (println (:summary (cli/parse-opts arguments specification)))
      (seq (problems options)) (do (run! println (problems options)) (System/exit 2))
      :else (let [running (start options)]
              (.addShutdownHook (Runtime/getRuntime) (Thread. ^Runnable (:stop running)))
              (println (str "listening on http://" (:host running) ":" (:port running)))
              @(promise)))))
