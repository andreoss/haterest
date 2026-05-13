(ns hypermedia.database
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str])
  (:import (java.sql DriverManager)))

(def root "scratch/db")

(defn- unique [prefix] (str prefix (System/nanoTime)))

(defn- run [& command]
  (apply shell/sh command))

(defn- tool? [name]
  (zero? (:exit (run "sh" "-c" (str "command -v " name " >/dev/null 2>&1")))))

(defn- reachable? [url]
  (try (with-open [_ (DriverManager/getConnection url)] true)
       (catch Exception _ false)))

(defn- wait-for [url attempts pause]
  (loop [left attempts]
    (cond
      (reachable? url) true
      (zero? left)     false
      :else            (do (Thread/sleep pause) (recur (dec left))))))

(defn- embedded [name url]
  {:name name :url url :stop (fn [])})

(defn h2 [] (embedded :h2 (str "jdbc:h2:mem:" (unique "e2e") ";DB_CLOSE_DELAY=-1")))

(defn sqlite []
  (.mkdirs (io/file root))
  (let [path (str root "/" (unique "e2e") ".db")]
    {:name :sqlite
     :url  (str "jdbc:sqlite:" path)
     :stop (fn [] (io/delete-file path true))}))

(defn hsqldb [] (embedded :hsqldb (str "jdbc:hsqldb:mem:" (unique "e2e") ";sql.syntax_pgs=false")))

(defn derby []
  (.mkdirs (io/file root))
  (let [path (str root "/" (unique "e2e"))]
    {:name :derby
     :url  (str "jdbc:derby:" path ";create=true")
     :stop (fn [] (run "rm" "-rf" path))}))

(defn- free-port []
  (with-open [socket (java.net.ServerSocket. 0)] (.getLocalPort socket)))

(defn postgres []
  (when (and (tool? "initdb") (tool? "pg_ctl"))
    (.mkdirs (io/file root))
    (let [cluster (str root "/" (unique "pg"))
          logfile (str cluster ".log")]
      (when (zero? (:exit (run "initdb" "-D" cluster "-U" "postgres" "--auth=trust" "-E" "UTF8")))
        (loop [tries 5]
          (let [port    (free-port)
                started (run "pg_ctl" "-D" cluster "-l" logfile "-w" "-o"
                             (str "-p " port " -k " (.getAbsolutePath (io/file cluster)) " -c listen_addresses=127.0.0.1")
                             "start")
                url     (str "jdbc:postgresql://127.0.0.1:" port "/postgres?user=postgres")]
            (cond
              (and (zero? (:exit started)) (wait-for url 30 200))
              {:name :postgres
               :url  url
               :stop (fn []
                       (run "pg_ctl" "-D" cluster "-m" "immediate" "-w" "stop")
                       (run "rm" "-rf" cluster)
                       (io/delete-file logfile true))}

              (pos? tries)
              (do (run "pg_ctl" "-D" cluster "-m" "immediate" "-w" "stop")
                  (recur (dec tries)))

              :else
              (do (run "rm" "-rf" cluster) nil))))))))

(defn- container-runtime []
  (first (filter #(and (tool? %) (zero? (:exit (run % "info")))) ["docker" "podman"])))

(defn- image? [runtime image]
  (str/includes? (:out (run runtime "images" "-q" image)) ""))

(defn mariadb []
  (when-let [runtime (container-runtime)]
    (when (seq (str/trim (:out (run runtime "images" "-q" "mariadb:11"))))
      (let [name (unique "e2e-mariadb-")
            port (free-port)
            run! (run runtime "run" "-d" "--rm" "--name" name
                      "-e" "MARIADB_ROOT_PASSWORD=secret"
                      "-e" "MARIADB_DATABASE=hypermedia"
                      "-p" (str "127.0.0.1:" port ":3306")
                      "mariadb:11")
            url  (str "jdbc:mariadb://127.0.0.1:" port "/hypermedia?user=root&password=secret")]
        (if (and (zero? (:exit run!)) (wait-for url 150 400))
          {:name :mariadb
           :url  url
           :stop (fn [] (run runtime "kill" name))}
          (do (run runtime "kill" name) nil))))))

(def builders
  [["h2" h2] ["sqlite" sqlite] ["hsqldb" hsqldb] ["derby" derby]
   ["postgres" postgres] ["mariadb" mariadb]])

(defn engines []
  (let [selected (some-> (System/getenv "HYPERMEDIA_ENGINES") (str/split #",") set)]
    (reduce (fn [acc [label build]]
              (if (and selected (not (contains? selected label)))
                (update acc :declined conj [label "not selected"])
                (if-let [engine (try (build) (catch Exception e
                                               (println "engine" label "refused:" (.getMessage e))
                                               nil))]
                  (update acc :ready conj engine)
                  (update acc :declined conj [label "unavailable"]))))
            {:ready [] :declined []}
            builders)))
