(ns hypermedia.trace-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hypermedia.trace :as trace]))

(defn- answering [status]
  (fn [request] {:status status :headers {} :body (trace/of request)}))

(deftest every-answer-carries-an-identity
  (let [response ((trace/identified (answering 200)) {:request-method :get :uri "/"})]
    (is (seq (get-in response [:headers trace/header])))
    (is (= (:body response) (get-in response [:headers trace/header])))))

(deftest two-requests-are-told-apart
  (let [handler (trace/identified (answering 200))]
    (is (not= (:body (handler {:request-method :get :uri "/"}))
              (:body (handler {:request-method :get :uri "/"}))))))

(deftest an-identity-the-caller-brought-is-kept
  (let [response ((trace/identified (answering 200))
                  {:request-method :get :uri "/" :headers {"x-request-id" "from-the-gateway"}})]
    (is (= "from-the-gateway" (get-in response [:headers trace/header])))
    (is (= "from-the-gateway" (:body response)))))

(deftest an-identity-the-caller-brought-is-bounded
  (let [long-one (apply str (repeat 500 "x"))
        response ((trace/identified (answering 200))
                  {:request-method :get :uri "/" :headers {"x-request-id" long-one}})]
    (is (= 64 (count (get-in response [:headers trace/header]))))))

(deftest a-blank-identity-is-replaced
  (let [response ((trace/identified (answering 200))
                  {:request-method :get :uri "/" :headers {"x-request-id" "  "}})]
    (is (not= "  " (get-in response [:headers trace/header])))
    (is (seq (get-in response [:headers trace/header])))))

(deftest a-request-is-logged-with-what-it-was-and-what-it-cost
  (let [line (with-out-str
               ((trace/logged (trace/identified (answering 201)))
                {:request-method :post :uri "/books" :headers {"x-request-id" "abc"}}))]
    (testing (str "logged: " (str/trim line))
      (is (str/starts-with? line "abc POST /books 201 "))
      (is (str/ends-with? (str/trim line) "ms")))))
