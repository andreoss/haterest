(ns hypermedia.client
  (:require [clojure.string :as str]
            [jsonista.core :as json])
  (:import (java.net URI)
           (java.net.http HttpClient HttpClient$Version HttpRequest HttpRequest$BodyPublishers
                          HttpResponse$BodyHandlers)
           (java.time Duration)))

(def ^:private client
  (-> (HttpClient/newBuilder)
      (.version HttpClient$Version/HTTP_1_1)
      (.connectTimeout (Duration/ofSeconds 5))
      (.build)))

(defn- header-of [response name]
  (.orElse (.firstValue (.headers response) name) nil))

(defn request
  [port method path & {:keys [body content-type accept headers]}]
  (let [publisher (if body
                    (HttpRequest$BodyPublishers/ofString body)
                    (HttpRequest$BodyPublishers/noBody))
        builder   (cond-> (-> (HttpRequest/newBuilder (URI/create (str "http://127.0.0.1:" port path)))
                              (.timeout (Duration/ofSeconds 10))
                              (.header "Connection" "close")
                              (.method (str/upper-case (name method)) publisher))
                    content-type (.header "Content-Type" content-type)
                    accept       (.header "Accept" accept))
        builder   (reduce (fn [b [k v]] (.header b k v)) builder headers)
        response  (.send client (.build builder) (HttpResponse$BodyHandlers/ofString))
        text      (.body response)]
    {:status   (.statusCode response)
     :location (header-of response "location")
     :etag     (header-of response "etag")
     :allow    (header-of response "allow")
     :type     (header-of response "content-type")
     :raw      text
     :body     (when (seq text)
                 (try (json/read-value text json/keyword-keys-object-mapper)
                      (catch Exception _ nil)))}))

(defn json-body [value] (json/write-value-as-string value))
