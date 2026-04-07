(ns hypermedia.store.memory
  (:require [hypermedia.store :as store]))

(defrecord Memory [state]
  store/Store
  (fetch [_ resource id]
    (get-in @state [(:name resource) id]))
  (query [_ resource criteria]
    (let [rows (vals (get @state (:name resource)))
          {:keys [where]} criteria]
      (vec (if (seq where)
             (filter (fn [row] (every? (fn [[k v]] (= v (get row k))) where)) rows)
             rows)))))

(defn store [data]
  (->Memory (atom data)))
