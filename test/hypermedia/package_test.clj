(ns hypermedia.package-test
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hypermedia.client :as client]
            [hypermedia.database :as database]))

(def ^:private listening #"listening on http://([^:]+):(\d+)")

(defn- built []
  (->> (.listFiles (io/file "target"))
       (filter #(str/ends-with? (.getName ^java.io.File %) "-standalone.jar"))
       first))

(defn- artefact []
  (or (built)
      (do (.println System/err "package: no jar yet, building one")
          (shell/sh "clojure" "-T:build" "uberjar")
          (built))))

(defn- announced [^java.io.BufferedReader reader deadline]
  (loop []
    (when (< (System/currentTimeMillis) deadline)
      (if-let [line (.readLine reader)]
        (if-let [[_ host port] (re-find listening line)]
          {:host host :port (Long/parseLong port)}
          (recur))
        (recur)))))

(defn- serving [jar url body]
  (let [builder (doto (ProcessBuilder. ["java" "-jar" (.getPath ^java.io.File jar)
                                        "-s" "example.edn" "-d" url
                                        "-p" "0" "--migrate"])
                  (.redirectErrorStream true))
        process (.start builder)]
    (try
      (with-open [reader (io/reader (.getInputStream process))]
        (if-let [address (announced reader (+ (System/currentTimeMillis) 60000))]
          (body address)
          (is false "the artefact never announced an address")))
      (finally (.destroyForcibly process) (.waitFor process)))))

(deftest the-artefact-runs-on-its-own
  (if-let [jar (artefact)]
    (let [engine (database/h2)]
      (try
        (serving
         jar (:url engine)
         (fn [{:keys [port]}]
           (testing "it binds the port it was given and announces the one it got"
             (is (pos? port)))
           (testing "it reports the store it opened"
             (is (= 200 (:status (client/request port :get "/health"))))
             (is (= "up" (get-in (client/request port :get "/health") [:body :status]))))
           (testing "it serves the schema it was pointed at"
             (let [root (:body (client/request port :get "/"))]
               (is (= "/books" (get-in root [:_links :rel:books :href])))
               (is (= "/authors" (get-in root [:_links :rel:authors :href])))))
           (testing "it writes and reads through the tables it derived"
             (let [author (client/request port :post "/authors"
                                          :body (client/json-body {:name "Ursula Le Guin"})
                                          :content-type "application/json")
                   book   (client/request port :post "/books"
                                          :body (client/json-body {:title "Earthsea"
                                                                   :year 1968
                                                                   :author (:location author)})
                                          :content-type "application/json")]
               (is (= 201 (:status author)))
               (is (= 201 (:status book)))
               (is (= "Ursula Le Guin" (get-in book [:body :_embedded :author :name])))
               (is (= 1 (get-in (client/request port :get "/books") [:body :page :totalElements])))))))
        (finally ((:stop engine)))))
    (do (.println System/err "package: no jar and none could be built; packaging not checked")
        (is false "the artefact could not be built"))))

(deftest the-artefact-carries-the-drivers-it-claims
  (if-let [jar (artefact)]
    (with-open [zip (java.util.zip.ZipFile. ^java.io.File jar)]
      (let [names (map #(.getName ^java.util.zip.ZipEntry %) (enumeration-seq (.entries zip)))
            holds (fn [prefix] (some #(str/starts-with? % prefix) names))]
        (doseq [[engine prefix] [["h2" "org/h2/"]
                                 ["postgresql" "org/postgresql/"]
                                 ["mariadb" "org/mariadb/"]
                                 ["hsqldb" "org/hsqldb/"]
                                 ["derby" "org/apache/derby/"]
                                 ["sqlite" "org/sqlite/"]]]
          (is (holds prefix) (str "no driver for " engine)))
        (is (holds "hypermedia/main.class") "the entry point was not compiled")))
    (is false "the artefact could not be built")))
