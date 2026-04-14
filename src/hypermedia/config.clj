(ns hypermedia.config
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [hypermedia.api :as api]
            [hypermedia.schema :as schema]))

(defn- source-of [location]
  (let [file (io/file location)]
    (cond
      (.isFile file)         file
      (io/resource location) (io/resource location)
      :else (throw (ex-info "schema not found" {:type ::not-found :location location})))))

(defn read-config [location]
  (edn/read-string (slurp (source-of location))))

(defn model [location]
  (schema/parse (read-config location)))

(defn api [location]
  (api/build (model location)))
