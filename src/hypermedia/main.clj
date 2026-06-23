(ns hypermedia.main
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.tools.cli :as cli]
            [hypermedia.api :as api]
            [hypermedia.config :as config]
            [hypermedia.server :as server]
            [hypermedia.sql :as sql]
            [hypermedia.trace :as trace]
            [hypermedia.store.jdbc :as jdbc-store]))

(def specification
  [["-s" "--schema PATH" "schema file or resource"]
   ["-d" "--database URL" "jdbc url"]
   ["-p" "--port PORT" "port to bind, zero for any" :default 8080 :parse-fn parse-long]
   ["-h" "--host HOST" "address to bind" :default "127.0.0.1"]
   ["-c" "--connections SIZE" "most connections to hold open" :parse-fn parse-long]
   [nil "--connection-timeout MS" "how long to wait for one" :parse-fn parse-long]
   [nil "--migrate" "derive and apply the schema before serving" :default false]
   [nil "--plan" "print what a migration would do, and do nothing" :default false]
   [nil "--quiet" "do not log requests" :default false]
   [nil "--drain MS" "how long to let requests finish when stopping" :parse-fn parse-long]
   [nil "--help"]])

(defn options [arguments]
  (let [{:keys [options errors]} (cli/parse-opts arguments specification)]
    (assoc options :errors errors)))

(defn problems [options]
  (cond-> (vec (:errors options))
    (str/blank? (:schema options))   (conj "a schema is required")
    (str/blank? (:database options)) (conj "a database url is required")))

(defn evolution [{:keys [schema database]}]
  (let [api   (config/api schema)
        store (jdbc-store/open {:url database :model (:model api)})]
    (try (jdbc-store/evolution (:datasource store) (:model api) (sql/dialect database))
         (finally (jdbc-store/close store)))))

(defn- type-name [code]
  (try (str/lower-case (.getName (java.sql.JDBCType/valueOf (int code))))
       (catch Exception _ (str code))))

(defn report [{:keys [statements refusals notes]}]
  (when (every? empty? [statements refusals notes])
    (println "the store already matches the schema"))
  (doseq [statement statements] (println statement))
  (doseq [{:keys [table column reason]} notes]
    (println (format "note: %s.%s %s" (name table) (name column) (name reason))))
  (doseq [{:keys [table column reason declared found]} refusals]
    (println (format "refused: %s.%s %s%s" (name table) (name column) (name reason)
                     (if declared
                       (format " (schema says %s, store holds %s)" (name declared) (type-name found))
                       ""))))
  (empty? refusals))

(defn start [{:keys [schema database port host migrate connections connection-timeout quiet drain]}]
  (let [api   (config/api schema)
        store (jdbc-store/open {:url database :model (:model api)
                                :statements (:statements api)
                                :pool {:size connections :timeout connection-timeout}
                                :migrate? (boolean migrate)})
        served  (cond-> (api/handler api store) (not quiet) (trace/logged))
        running (server/start served {:port (or port 8080)
                                      :host (or host "127.0.0.1")
                                      :drain (or drain server/default-drain)})]
    (assoc running
           :store store
           :stop (fn []
                   ((:stop running))
                   (jdbc-store/close store)))))

(defn detach [running]
  (jdbc-store/close (:store running)))

(defn- reason [^Exception e]
  (let [root (loop [c e] (if (.getCause c) (recur (.getCause c)) c))]
    (first (str/split-lines (str (or (.getMessage root) (.getName (class root))))))))

(defn refusal [^Exception e options]
  (case (:type (ex-data e))
    :hypermedia.config/not-found
    {:code 2 :lines [(str "no schema at " (:schema options))]}

    :hypermedia.store.jdbc/refused
    {:code 3 :refusals (:refusals (ex-data e))}

    :hypermedia.store.jdbc/unfinished
    {:code 3 :lines (into ["the store could not be brought to the schema"]
                          (map :message (:failures (ex-data e))))}

    {:code 4 :lines [(str "cannot reach the store at " (:database options))
                     (str "  " (reason e))]}))

(defn- announce [{:keys [lines refusals]}]
  (run! println lines)
  (when refusals (report {:statements [] :notes [] :refusals refusals})))

(defn -main [& arguments]
  (let [options (options arguments)]
    (cond
      (:help options) (println (:summary (cli/parse-opts arguments specification)))
      (seq (problems options)) (do (run! println (problems options)) (System/exit 2))
      (:plan options) (System/exit (if (report (evolution options)) 0 3))
      :else
      (let [outcome (try {:running (start options)}
                         (catch Exception e {:failed (refusal e options)}))]
        (if-let [failed (:failed outcome)]
          (do (announce failed) (System/exit (:code failed)))
          (let [running (:running outcome)]
            (.addShutdownHook (Runtime/getRuntime) (Thread. ^Runnable (:stop running)))
            (println (str "listening on http://" (:host running) ":" (:port running)))
            @(promise)))))))
