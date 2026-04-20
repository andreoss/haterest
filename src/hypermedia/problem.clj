(ns hypermedia.problem
  (:require [jsonista.core :as json]))

(def media-type "application/problem+json")

(def ^:private mapper (json/object-mapper {:encode-key-fn name}))

(defn document [{:keys [status title detail instance errors]}]
  (cond-> {:type "about:blank" :title title :status status}
    detail       (assoc :detail detail)
    instance     (assoc :instance instance)
    (seq errors) (assoc :errors (vec errors))))

(defn response [problem]
  {:status  (:status problem)
   :headers {"Content-Type" (str media-type ";charset=utf-8")}
   :body    (json/write-value-as-string problem mapper)})

(defn of [status title options]
  (response (document (assoc options :status status :title title))))
