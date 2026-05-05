(ns hypermedia.conformance
  (:require [clojure.test :refer [deftest is testing]]
            [hypermedia.api :as api]
            [hypermedia.rel :as rel]
            [hypermedia.uri :as uri]
            [jsonista.core :as json]))

(defn probe [handler method path]
  (let [[uri query] (clojure.string/split path #"\?" 2)
        response    (handler {:request-method method :uri uri :query-string query :headers {}})]
    (assoc response :document (some-> (:body response)
                                      (json/read-value json/keyword-keys-object-mapper)))))

(defn absent-identity [resource]
  (case (get-in resource [:fields (:identity resource) :type])
    :long    "987654321"
    :uuid    "00000000-0000-0000-0000-0000000000ff"
    :decimal "0"
    :double  "0"
    "absent"))

(defn- resource-tests [label api-sym store-form model handler resource]
  (let [path    (:path resource)
        absent  (absent-identity resource)
        rel-key (rel/curied (:curie model) (:collection resource))]
    `(deftest ~(symbol (str "conforms-" (name label) "-" (name (:name resource))))
       (let [~handler (api/handler ~api-sym ~store-form)]
         (testing "the root links the collection"
           (is (= ~path (get-in (:document (probe ~handler :get "/")) [:_links ~rel-key :href]))))
         (testing "the collection answers a page"
           (let [response# (probe ~handler :get ~path)]
             (is (= 200 (:status response#)))
             (is (map? (get-in response# [:document :page])))))
         (testing "the collection states what it allows"
           (is (= "GET, POST, OPTIONS" (get-in (probe ~handler :options ~path) [:headers "Allow"]))))
         (testing "the collection points at its profile"
           (is (= ~(:profile-path resource)
                  (get-in (:document (probe ~handler :get ~path)) [:_links :profile :href]))))
         (testing "the profile describes the resource"
           (is (= 200 (:status (probe ~handler :get ~(:profile-path resource))))))
         (testing "an item that is not there is not found"
           (is (= 404 (:status (probe ~handler :get ~(str path "/" absent))))))
         (testing "an item states what it allows"
           (is (= "GET, PUT, PATCH, DELETE, OPTIONS"
                  (get-in (probe ~handler :options ~(str path "/" absent)) [:headers "Allow"]))))
         ~@(for [[k relation] (:relations resource)]
             `(testing ~(str "the " (name k) " association is addressable")
                (is (= 404 (:status (probe ~handler :get
                                           ~(uri/expand (:path relation)
                                                        {(:identity resource) absent})))))))
         ~@(when (seq (:searches resource))
             (let [links (gensym "links")]
               [`(testing "the searches are listed"
                   (let [~links (get-in (:document (probe ~handler :get ~(:search-path resource)))
                                        [:_links])]
                     ~@(for [[k search] (:searches resource)]
                         `(is (= ~(:template search)
                                 (get-in ~links [~(rel/curied (:curie model) k) :href]))))))]))))))

(defmacro defconformance [label api-sym store-form]
  (let [model   (:model (deref (resolve api-sym)))
        handler (gensym "handler")]
    `(do
       ~@(for [k (:order model)]
           (resource-tests label api-sym store-form model handler (get-in model [:resources k]))))))
