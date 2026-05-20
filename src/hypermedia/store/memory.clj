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

(defn- rows-of [state relation]
  (get @state (get-in relation [:join :table]) #{}))

(defn- pairs [state relation]
  (let [join (:join relation)]
    (map (fn [row] [(get row (:via-column join)) (get row (:target-via-column join))])
         (rows-of state relation))))

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
      (swap! state assoc-in [(:name resource) id] (assoc row store/version-key 0))
      (store/fetch this resource id)))
  (replace! [this resource id row expected]
    (known-fields resource (keys row))
    (let [current (store/fetch this resource id)]
      (if (and expected (not= expected (store/version-of current)))
        {:outcome :stale}
        (let [stored (assoc row
                            (:identity resource) id
                            store/version-key (if current (inc (store/version-of current)) 0))]
          (swap! state assoc-in [(:name resource) id] stored)
          {:outcome (if current :written :created) :row (store/fetch this resource id)}))))
  (amend! [this resource id row expected]
    (known-fields resource (keys row))
    (let [current (store/fetch this resource id)]
      (cond
        (nil? current) {:outcome :absent}
        (and expected (not= expected (store/version-of current))) {:outcome :stale}
        :else
        (do (swap! state update-in [(:name resource) id]
                   (fn [held] (-> (merge held (dissoc row (:identity resource)))
                                  (assoc store/version-key (inc (store/version-of held))))))
            {:outcome :written :row (store/fetch this resource id)}))))
  (erase! [this resource id expected]
    (let [current (store/fetch this resource id)]
      (cond
        (nil? current) {:outcome :absent}
        (and expected (not= expected (store/version-of current))) {:outcome :stale}
        :else (do (swap! state update (:name resource) dissoc id) {:outcome :erased}))))
  (transact [this body] (locking state (body this)))
  (linked [this _ target relation owner-id criteria]
    (let [ids (into #{} (keep (fn [[o t]] (when (= o owner-id) t))) (pairs state relation))]
      (if (seq ids)
        (store/query this target (assoc criteria :where {(:identity target) ids}))
        [])))
  (linked-total [this _ target relation owner-id]
    (count (filter (fn [[o t]] (and (= o owner-id) (some? (store/fetch this target t))))
                   (pairs state relation))))
  (links-of [_ _owner _target relation owner-ids]
    (let [wanted (set owner-ids)]
      (vec (sort-by str (filter (fn [[o _]] (contains? wanted o)) (pairs state relation))))))
  (link! [_ _owner _target relation owner-id target-ids]
    (let [join (:join relation)]
      (swap! state update (:table join) (fnil into #{})
             (map (fn [t] {(:via-column join) owner-id (:target-via-column join) t}) target-ids))
      (count target-ids)))
  (unlink! [_ _owner _target relation owner-id target-ids]
    (let [join   (:join relation)
          doomed (when target-ids (set target-ids))
          gone   (filter (fn [row] (and (= owner-id (get row (:via-column join)))
                                        (or (nil? doomed)
                                            (contains? doomed (get row (:target-via-column join))))))
                         (rows-of state relation))]
      (swap! state update (:table join) (fnil (partial reduce disj) #{}) gone)
      (count gone))))

(defn store [data]
  (->Memory (atom data)))
