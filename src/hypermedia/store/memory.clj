(ns hypermedia.store.memory
  (:require [hypermedia.store :as store]))

(defn- matching [rows where]
  (if (seq where)
    (filter (fn [row] (every? (fn [[k v]] (= v (get row k))) where)) rows)
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

(defn- known-fields [resource where]
  (when-let [unknown (seq (remove #(contains? (:fields resource) %) (keys where)))]
    (throw (ex-info "unknown field" {:type :hypermedia.sql/unknown-field
                                     :resource (:name resource)
                                     :fields (vec unknown)}))))

(defrecord Memory [state]
  store/Store
  (fetch [_ resource id]
    (get-in @state [(:name resource) id]))
  (query [_ resource {:keys [where order limit offset]}]
    (known-fields resource where)
    (-> (vals (get @state (:name resource)))
        (matching where)
        (ordered order)
        (sliced limit offset)))
  (total [_ resource {:keys [where]}]
    (known-fields resource where)
    (count (matching (vals (get @state (:name resource))) where)))
  (probe [_] (map? @state)))

(defn store [data]
  (->Memory (atom data)))
