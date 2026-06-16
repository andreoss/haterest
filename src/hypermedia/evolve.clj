(ns hypermedia.evolve
  (:require [clojure.string :as str]
            [hypermedia.sql :as sql]
            [next.jdbc :as jdbc]))

(def accepted
  {:uuid    #{-2 1 12 1111}
   :string  #{1 12}
   :text    #{-1 12 2005}
   :long    #{-5 4 5}
   :double  #{6 7 8}
   :decimal #{2 3 6}
   :boolean #{-7 -6 4 16}
   :instant #{12 93 2014}
   :date    #{12 91}})

(defn- reported [^java.sql.DatabaseMetaData metadata pattern]
  (with-open [rows (.getColumns metadata nil nil pattern nil)]
    (loop [found {}]
      (if (.next rows)
        (recur (assoc found
                      (keyword (str/lower-case (.getString rows "COLUMN_NAME")))
                      {:code      (.getInt rows "DATA_TYPE")
                       :nullable? (not= "NO" (.getString rows "IS_NULLABLE"))}))
        found))))

(defn- columns-of [metadata table]
  (reduce (fn [found pattern] (merge found (reported metadata pattern)))
          {}
          [(name table) (str/upper-case (name table))]))

(defn tables-of [model]
  (into (mapv #(:table (get-in model [:resources %])) (:order model))
        (for [k (:order model)
              [_ relation] (:relations (get-in model [:resources k]))
              :when (= :many-to-many (:kind relation))]
          (get-in relation [:join :table]))))

(defn live [datasource model]
  (with-open [connection (jdbc/get-connection datasource)]
    (let [metadata (.getMetaData connection)]
      (into {} (for [table (tables-of model)
                     :let [columns (columns-of metadata table)]
                     :when (seq columns)]
                 [table columns])))))

(defn- declared [model]
  (into {}
        (for [k (:order model)
              :let [resource (get-in model [:resources k])]]
          [(:table resource)
           (into {sql/version-column {:type :long :required? true :version? true}}
                 (for [field (:field-order resource)
                       :let [spec (get-in resource [:fields field])]]
                   [(:column spec) {:type (:type spec)
                                    :required? (:required? spec)
                                    :field field}]))])))

(defn- holds-rows? [datasource table]
  (boolean (some-> (jdbc/execute-one! datasource [(str "SELECT COUNT(*) AS n FROM " (sql/quoted table))])
                   vals first pos?)))

(defn- joins-wanted [model]
  (into {} (for [k (:order model)
                 [_ relation] (:relations (get-in model [:resources k]))
                 :when (= :many-to-many (:kind relation))]
             [(get-in relation [:join :table]) {}])))

(defn plan [model dialect live occupied?]
  (let [wanted (merge (joins-wanted model) (declared model))]
    (reduce
     (fn [acc [table columns]]
       (let [resource (sql/table-of model table)
             present  (get live table)]
         (if (nil? present)
           (update acc :statements conj (sql/creation-of model dialect table))
           (reduce
            (fn [acc [column {:keys [type required? field version?]}]]
              (let [actual (get present column)]
                (cond
                  (and (nil? actual) version?)
                  (update acc :statements conj (sql/add-version-column dialect table))

                  (and (nil? actual) (not required?))
                  (update acc :statements conj (sql/add-column dialect resource field))

                  (nil? actual)
                  (cond
                    (not (sql/adds-required-column? dialect))
                    (update acc :refusals conj
                            {:table table :column column :reason :cannot-add-required})

                    (occupied? table)
                    (update acc :refusals conj
                            {:table table :column column :reason :required-column-on-rows})

                    :else
                    (update acc :statements conj (sql/add-column dialect resource field)))

                  (not (contains? (get accepted type #{}) (:code actual)))
                  (update acc :refusals conj
                          {:table table :column column :reason :type-differs
                           :declared type :found (:code actual)})

                  :else acc)))
            acc
            columns)))) 
     (reduce
      (fn [acc [table columns]]
        (if-let [present (get live table)]
          (reduce (fn [acc [column {:keys [nullable?]}]]
                    (cond
                      (contains? columns column) acc
                      nullable? (update acc :notes conj {:table table :column column :reason :not-declared})
                      :else     (update acc :refusals conj
                                        {:table table :column column :reason :required-and-not-declared})))
                  acc present)
          acc))
      {:statements [] :refusals [] :notes []}
      wanted)
     wanted)))

(defn evolution [datasource model dialect]
  (let [present (live datasource model)]
    (plan model dialect present #(holds-rows? datasource %))))
