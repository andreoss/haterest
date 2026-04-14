(ns hypermedia.config-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [hypermedia.config :as config]))

(deftest loads-a-schema-from-a-resource
  (let [api (config/api "example.edn")]
    (is (= [:author :book] (:order (:model api))))
    (is (seq (:routes api)))))

(deftest loads-a-schema-from-a-path
  (.mkdirs (io/file "scratch"))
  (let [path (str "scratch/" (gensym "schema") ".edn")]
    (spit path (pr-str {:resources {:note {:fields {:id {:type :long :identity true}}}}}))
    (try (is (= [:note] (:order (:model (config/api path)))))
         (finally (io/delete-file path true)))))

(deftest reports-a-missing-source
  (is (thrown? clojure.lang.ExceptionInfo (config/api "nowhere.edn"))))

(deftest reports-an-invalid-schema
  (.mkdirs (io/file "scratch"))
  (let [path (str "scratch/" (gensym "schema") ".edn")]
    (spit path (pr-str {:resources {:note {:fields {:id {:type :wat}}}}}))
    (try (is (thrown? clojure.lang.ExceptionInfo (config/api path)))
         (finally (io/delete-file path true)))))
