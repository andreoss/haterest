(ns hypermedia.trace
  (:require [clojure.string :as str]))

(def header "X-Request-Id")

(def ^:private lookup (str/lower-case header))

(defn- given [request]
  (let [supplied (get-in request [:headers lookup])]
    (when-not (str/blank? supplied)
      (subs supplied 0 (min 64 (count supplied))))))

(defn- minted []
  (subs (str (random-uuid)) 0 12))

(defn identified [handler]
  (fn [request]
    (let [id       (or (given request) (minted))
          response (handler (assoc request ::id id))]
      (cond-> response
        (map? response) (update :headers assoc header id)))))

(defn of [request] (::id request))

(defn logged [handler]
  (fn [request]
    (let [began    (System/nanoTime)
          response (handler request)
          took     (quot (- (System/nanoTime) began) 1000000)]
      (println (format "%s %s %s %s %sms"
                       (or (get-in response [:headers header]) (of request) "-")
                       (str/upper-case (name (:request-method request)))
                       (:uri request)
                       (:status response)
                       took))
      (flush)
      response)))
