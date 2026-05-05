(ns hypermedia.conformance-test
  (:require [clojure.test :refer [deftest is]]
            [hypermedia.api :as api]
            [hypermedia.conformance :refer [defconformance]]
            [hypermedia.config :as config]
            [hypermedia.store.memory :as memory]))

(api/defapi demo
  {:resources
   {:author {:fields    {:id {:type :long :identity true} :name {:type :string :required true}}
             :relations {:books {:kind :has-many :target :book :via :author-id}}
             :searches  {:by-name {:predicates [:name]}}}
    :book   {:fields      {:id        {:type :long :identity true}
                           :title     {:type :string :required true}
                           :author-id {:type :long}}
             :relations   {:author {:kind :belongs-to :target :author :via :author-id}}
             :projections {:summary {:fields [:title]}}}}})

(defconformance declared demo (memory/store {:author {} :book {}}))

(def shipped (config/api "example.edn"))

(defconformance shipped shipped (memory/store {:author {} :book {}}))

(deftest the-suite-was-generated-not-written
  (is (= 4 (count (filter #(clojure.string/starts-with? (name %) "conforms-")
                          (keys (ns-publics 'hypermedia.conformance-test)))))))

(deftest a-schema-is-expanded-before-the-first-request
  (is (string? (get-in demo [:statements :book :by-identity])))
  (is (= "SELECT \"id\", \"title\", \"author_id\" FROM \"books\" WHERE \"id\" = ?"
         (get-in demo [:statements :book :by-identity])))
  (let [expansion (macroexpand-1 '(hypermedia.api/defapi x {:resources {:a {:fields {:id {:type :long :identity true}}}}}))]
    (is (some #(= "SELECT \"id\" FROM \"as\" WHERE \"id\" = ?" %)
              (tree-seq coll? seq expansion)))
    (is (some #(= "/as/{id}" %) (tree-seq coll? seq expansion)))))
