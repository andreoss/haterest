(ns build
  (:require [clojure.tools.build.api :as b]))

(def version (or (System/getenv "VERSION") "0.1.0"))

(def main 'hypermedia.main)

(def class-dir "target/classes")

(def uber-file (format "target/hypermedia-%s-standalone.jar" version))

(defn- basis []
  (b/create-basis {:project "deps.edn" :aliases [:drivers]}))

(defn clean [_]
  (b/delete {:path "target"}))

(defn uberjar [_]
  (clean nil)
  (b/copy-dir {:src-dirs ["src" "resources"] :target-dir class-dir})
  (b/compile-clj {:basis (basis) :ns-compile [main] :class-dir class-dir})
  (b/uber {:class-dir class-dir :uber-file uber-file :basis (basis) :main main})
  (println uber-file))
