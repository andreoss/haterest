(ns hypermedia.store.memory
  (:require [hypermedia.store :as store]))

(defn- holds? [expected actual]
  (if (coll? expected) (contains? (set expected) actual) (= expected actual)))

(defn- matching [rows where]
  (if (seq where)
    (filter (fn [row] (every? (fn [[k v]] (holds? v (get row k))) where)) rows)
    rows))

(defn- ordered [rows order]
  (reduce (fn [acc [field direction]]
            (let [sorted (sort-by #(get % field) compare acc)]
              (vec (if (= :desc direction) (reverse sorted) sorted))))
          (vec rows)
          (reverse order)))

(defn- sliced [rows limit offset]
  (cond->> rows
    offset (drop offset)
    limit  (take limit)
    true   vec))

(defn- known-fields [resource fields]
  (when-let [unknown (seq (remove #(contains? (:fields resource) %) fields))]
    (throw (ex-info "unknown field" {:type :hypermedia.sql/unknown-field
                                     :resource (:name resource)
                                     :fields (vec unknown)}))))

(defrecord Memory [state]
  store/Store
  (fetch [_ resource id]
    (get-in @state [(:name resource) id]))
  (query [_ resource {:keys [where order limit offset]}]
    (known-fields resource (keys where))
    (-> (vals (get @state (:name resource)))
        (matching where)
        (ordered order)
        (sliced limit offset)))
  (total [_ resource {:keys [where]}]
    (known-fields resource (keys where))
    (count (matching (vals (get @state (:name resource))) where)))
  (probe [_] (map? @state))
  (create! [this resource row]
    (known-fields resource (keys row))
    (let [id (get row (:identity resource))]
      (swap! state assoc-in [(:name resource) id] row)
      (store/fetch this resource id)))
  (replace! [this resource id row]
    (known-fields resource (keys row))
    (let [existed (some? (store/fetch this resource id))
          stored  (assoc row (:identity resource) id)]
      (swap! state assoc-in [(:name resource) id] stored)
      {:created? (not existed) :row (store/fetch this resource id)}))
  (amend! [this resource id row]
    (known-fields resource (keys row))
    (when (store/fetch this resource id)
      (swap! state update-in [(:name resource) id] merge (dissoc row (:identity resource)))
      (store/fetch this resource id)))
  (erase! [this resource id]
    (if (store/fetch this resource id)
      (do (swap! state update (:name resource) dissoc id) true)
      false))
  (transact [this body] (locking state (body this))))

(defn store [data]
  (->Memory (atom data)))
