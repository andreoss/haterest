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

(defn- patterns [table]
  [(name table) (str/upper-case (name table))])

(defn- reported-columns [^java.sql.DatabaseMetaData metadata pattern]
  (with-open [rows (.getColumns metadata nil nil pattern nil)]
    (loop [found {}]
      (if (.next rows)
        (recur (if-let [column (.getString rows "COLUMN_NAME")]
                 (assoc found (keyword (str/lower-case column))
                        {:code      (.getInt rows "DATA_TYPE")
                         :nullable? (not= "NO" (.getString rows "IS_NULLABLE"))})
                 found))
        found))))

(defn- reported-indexes [^java.sql.DatabaseMetaData metadata pattern]
  (try
    (with-open [rows (.getIndexInfo metadata nil nil pattern false true)]
      (loop [found #{}]
        (if (.next rows)
          (recur (let [column (.getString rows "COLUMN_NAME")]
                   (if (and column (= 1 (.getShort rows "ORDINAL_POSITION")))
                     (conj found (keyword (str/lower-case column)))
                     found)))
          found)))
    (catch Exception _ #{})))

(defn tables-of [model]
  (into (mapv #(:table (get-in model [:resources %])) (:order model))
        (for [k (:order model)
              [_ relation] (:relations (get-in model [:resources k]))
              :when (= :many-to-many (:kind relation))]
          (get-in relation [:join :table]))))

(defn live [datasource model]
  (with-open [connection (jdbc/get-connection datasource)]
    (let [metadata (.getMetaData connection)]
      (reduce (fn [acc table]
                (let [columns (reduce (fn [m p] (merge m (reported-columns metadata p)))
                                      {} (patterns table))]
                  (if (seq columns)
                    (-> acc
                        (assoc-in [:columns table] columns)
                        (assoc-in [:indexes table]
                                  (reduce (fn [s p] (into s (reported-indexes metadata p)))
                                          #{} (patterns table))))
                    acc)))
              {:columns {} :indexes {}}
              (tables-of model)))))

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

(defn- joins-declared [model]
  (into {}
        (for [k (:order model)
              [_ relation] (:relations (get-in model [:resources k]))
              :when (= :many-to-many (:kind relation))
              :let [owner  (get-in model [:resources k])
                    target (get-in model [:resources (:target relation)])
                    join   (:join relation)]]
          [(:table join)
           {(:via-column join)
            {:type (get-in owner [:fields (:identity owner) :type]) :required? true :join? true}
            (:target-via-column join)
            {:type (get-in target [:fields (:identity target) :type]) :required? true :join? true}}])))

(defn- column-difference [acc model dialect present occupied? table columns]
  (let [resource (sql/table-of model table)]
    (reduce
     (fn [acc [column {:keys [type required? field version? join?]}]]
       (let [actual (get present column)]
         (cond
           (and (nil? actual) join?)
           (update acc :refusals conj {:table table :column column :reason :join-column-missing})

           (and (nil? actual) version?)
           (update acc :alters conj (sql/add-version-column dialect table))

           (and (nil? actual) (not required?))
           (update acc :alters conj (sql/add-column dialect resource field))

           (nil? actual)
           (cond
             (not (sql/adds-required-column? dialect))
             (update acc :refusals conj {:table table :column column :reason :cannot-add-required})

             (occupied? table)
             (update acc :refusals conj {:table table :column column :reason :required-column-on-rows})

             :else
             (update acc :alters conj (sql/add-column dialect resource field)))

           (not (contains? (get accepted type #{}) (:code actual)))
           (update acc :refusals conj {:table table :column column :reason :type-differs
                                       :declared type :found (:code actual)})

           :else acc)))
     acc columns)))

(defn- surplus [acc table columns present]
  (reduce (fn [acc [column {:keys [nullable?]}]]
            (cond
              (contains? columns column) acc
              nullable? (update acc :notes conj {:table table :column column :reason :not-declared})
              :else     (update acc :refusals conj
                                {:table table :column column :reason :required-and-not-declared})))
          acc present))

(defn plan [model dialect live occupied?]
  (let [wanted (merge (joins-declared model) (declared model))
        shape  (:columns live)
        held   (:indexes live)
        base   (reduce
                (fn [acc [table columns]]
                  (if-let [present (get shape table)]
                    (-> acc
                        (column-difference model dialect present occupied? table columns)
                        (surplus table columns present))
                    (update acc :creates conj (sql/creation-of model dialect table))))
                {:creates [] :alters [] :indexes [] :refusals [] :notes []}
                wanted)
        full   (reduce
                (fn [acc [table columns]]
                  (reduce (fn [acc column]
                            (if (contains? (get held table #{}) column)
                              acc
                              (update acc :indexes conj (sql/index-of dialect table column))))
                          acc columns))
                base
                (sort-by key (sql/indexed-columns model dialect)))]
    (assoc full :statements (vec (concat (:creates full) (:alters full) (:indexes full))))))

(defn evolution [datasource model dialect]
  (let [present (live datasource model)]
    (plan model dialect present
          #(boolean (some-> (jdbc/execute-one! datasource
                                               [(str "SELECT COUNT(*) AS n FROM " (sql/quoted %))])
                            vals first pos?)))))
