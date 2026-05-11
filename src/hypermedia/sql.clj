(ns hypermedia.sql
  (:require [clojure.string :as str]))

(def ansi-types
  {:long    "BIGINT"
   :string  "VARCHAR(255)"
   :text    "CLOB"
   :uuid    "VARCHAR(36)"
   :boolean "BOOLEAN"
   :double  "DOUBLE PRECISION"
   :decimal "DECIMAL(19,4)"
   :instant "TIMESTAMP"
   :date    "DATE"})

(def dialect-types
  {:ansi     ansi-types
   :h2       (assoc ansi-types :uuid "UUID" :instant "TIMESTAMP WITH TIME ZONE")
   :postgres (assoc ansi-types :uuid "UUID" :text "TEXT" :instant "TIMESTAMP WITH TIME ZONE")
   :sqlite   {:long    "INTEGER"
              :string  "TEXT"
              :text    "TEXT"
              :uuid    "TEXT"
              :boolean "INTEGER"
              :double  "REAL"
              :decimal "NUMERIC"
              :instant "TEXT"
              :date    "TEXT"}})

(defn dialect [url]
  (condp #(str/starts-with? %2 %1) (str url)
    "jdbc:h2:"         :h2
    "jdbc:sqlite:"     :sqlite
    "jdbc:postgresql:" :postgres
    :ansi))

(defn column-type [dialect type]
  (or (get-in dialect-types [dialect type])
      (get ansi-types type)
      (throw (ex-info "unknown type" {:type ::unknown-type :dialect dialect :field-type type}))))

(defn quoted [identifier]
  (str \" (str/replace (name identifier) \" \_) \"))

(defn columns-of [resource]
  (mapv #(get-in resource [:fields % :column]) (:field-order resource)))

(defn- column-clause [dialect resource field]
  (let [spec (get-in resource [:fields field])]
    (str (quoted (:column spec)) " " (column-type dialect (:type spec))
         (when (:required? spec) " NOT NULL"))))

(defn- foreign-keys [model resource]
  (for [[_ relation] (:relations resource)
        :when (= :belongs-to (:kind relation))
        :let [target (get-in model [:resources (:target relation)])]
        :when target]
    (str "FOREIGN KEY (" (quoted (get-in resource [:fields (:via relation) :column]))
         ") REFERENCES " (quoted (:table target))
         " (" (quoted (get-in target [:fields (:identity target) :column])) ")")))

(defn- depends-on [model resource]
  (into #{} (keep (fn [[_ relation]]
                    (when (and (= :belongs-to (:kind relation))
                               (not= (:target relation) (:name resource)))
                      (:target relation))))
        (:relations resource)))

(defn creation-order [model]
  (loop [pending (vec (:order model)) emitted [] seen #{}]
    (if (empty? pending)
      emitted
      (let [ready (filterv #(every? seen (depends-on model (get-in model [:resources %]))) pending)
            ready (if (seq ready) ready [(first pending)])]
        (recur (vec (remove (set ready) pending))
               (into emitted ready)
               (into seen ready))))))

(defn create-table [model dialect resource]
  (let [identity-column (quoted (get-in resource [:fields (:identity resource) :column]))
        parts (concat (map #(column-clause dialect resource %) (:field-order resource))
                      [(str "PRIMARY KEY (" identity-column ")")]
                      (foreign-keys model resource))]
    (str "CREATE TABLE IF NOT EXISTS " (quoted (:table resource))
         " (" (str/join ", " parts) ")")))

(declare join-tables)

(defn ddl [model dialect]
  (into (mapv #(create-table model dialect (get-in model [:resources %])) (creation-order model))
        (join-tables model dialect)))

(defn projection [resource]
  (str "SELECT " (str/join ", " (map quoted (columns-of resource)))
       " FROM " (quoted (:table resource))))

(defn select-by-identity [resource]
  (str (projection resource)
       " WHERE " (quoted (get-in resource [:fields (:identity resource) :column])) " = ?"))

(defn- check-fields [resource fields]
  (when-let [unknown (seq (remove #(contains? (:fields resource) %) fields))]
    (throw (ex-info "unknown field" {:type ::unknown-field
                                     :resource (:name resource)
                                     :fields (vec unknown)}))))

(defn- order-clause
  ([resource order] (order-clause resource order ""))
  ([resource order prefix]
   (when (seq order)
     (check-fields resource (map first order))
     (str " ORDER BY "
          (str/join ", " (for [[field direction] order]
                           (str prefix (quoted (get-in resource [:fields field :column]))
                                (if (= :desc direction) " DESC" " ASC"))))))))

(defn- values-of [value]
  (if (coll? value) (vec (sort-by str value)) [value]))

(defn- predicate [resource [field value]]
  (let [column (quoted (get-in resource [:fields field :column]))]
    (if (coll? value)
      (if (empty? value)
        {:sql "1 = 0" :params []}
        {:sql (str column " IN (" (str/join ", " (repeat (count value) "?")) ")")
         :params (values-of value)})
      {:sql (str column " = ?") :params [value]})))

(defn- where-clause [resource where]
  (let [parts (mapv #(predicate resource %) where)]
    {:sql    (when (seq parts) (str " WHERE " (str/join " AND " (map :sql parts))))
     :params (vec (mapcat :params parts))}))

(defn select [resource {:keys [where order limit offset]}]
  (check-fields resource (keys where))
  (let [clause (where-clause resource where)]
    (into [(str (projection resource)
                (:sql clause)
                (order-clause resource order)
                (when limit " LIMIT ?")
                (when offset " OFFSET ?"))]
          (concat (:params clause) (when limit [limit]) (when offset [offset])))))

(defn count-of [resource {:keys [where]}]
  (check-fields resource (keys where))
  (let [clause (where-clause resource where)]
    (into [(str "SELECT COUNT(*) AS \"total\" FROM " (quoted (:table resource)) (:sql clause))]
          (:params clause))))

(defn- write-columns [resource row]
  (filterv #(contains? row %) (:field-order resource)))

(defn insert [resource row]
  (check-fields resource (keys row))
  (let [fields (write-columns resource row)]
    (into [(str "INSERT INTO " (quoted (:table resource))
                " (" (str/join ", " (map #(quoted (get-in resource [:fields % :column])) fields)) ")"
                " VALUES (" (str/join ", " (repeat (count fields) "?")) ")")]
          (map #(get row %) fields))))

(defn update-by-identity [resource id row]
  (check-fields resource (keys row))
  (let [fields (remove #(= % (:identity resource)) (write-columns resource row))]
    (into [(str "UPDATE " (quoted (:table resource)) " SET "
                (str/join ", " (map #(str (quoted (get-in resource [:fields % :column])) " = ?") fields))
                " WHERE " (quoted (get-in resource [:fields (:identity resource) :column])) " = ?")]
          (conj (mapv #(get row %) fields) id))))

(defn delete-by-identity [resource id]
  [(str "DELETE FROM " (quoted (:table resource))
        " WHERE " (quoted (get-in resource [:fields (:identity resource) :column])) " = ?")
   id])

(def ^:private encoders
  {:sqlite {:uuid str :boolean #(if % 1 0) :instant str :date str}})

(def ^:private readers
  {:uuid    (fn [v] (if (uuid? v) v (java.util.UUID/fromString (str v))))
   :boolean (fn [v] (cond (boolean? v) v
                          (number? v)  (not (zero? (long v)))
                          :else        (Boolean/parseBoolean (str v))))
   :instant (fn [v] (condp instance? v
                      java.time.Instant        v
                      java.time.OffsetDateTime (.toInstant ^java.time.OffsetDateTime v)
                      java.sql.Timestamp       (.toInstant ^java.sql.Timestamp v)
                      (java.time.Instant/parse (str v))))
   :date    (fn [v] (condp instance? v
                      java.time.LocalDate v
                      java.sql.Date       (.toLocalDate ^java.sql.Date v)
                      (java.time.LocalDate/parse (str v))))
   :decimal (fn [v] (bigdec v))})

(defn encode [dialect type value]
  (if-let [f (and (some? value) (get-in encoders [dialect type]))]
    (f value)
    value))

(defn decode [_ type value]
  (if-let [f (and (some? value) (get readers type))]
    (f value)
    value))

(defn statements [model]
  (into {}
        (for [k (:order model)
              :let [resource (get-in model [:resources k])]]
          [k {:projection   (projection resource)
              :by-identity  (select-by-identity resource)
              :delete       (first (delete-by-identity resource nil))
              :count        (first (count-of resource {}))}])))

(defn join-tables [model dialect]
  (->> (for [k (:order model)
             :let [owner (get-in model [:resources k])]
             [_ relation] (:relations owner)
             :when (= :many-to-many (:kind relation))
             :let [join   (:join relation)
                   target (get-in model [:resources (:target relation)])]]
         [(:table join)
          (str "CREATE TABLE IF NOT EXISTS " (quoted (:table join))
               " (" (quoted (:via-column join)) " "
               (column-type dialect (get-in owner [:fields (:identity owner) :type])) " NOT NULL, "
               (quoted (:target-via-column join)) " "
               (column-type dialect (get-in target [:fields (:identity target) :type])) " NOT NULL, "
               "PRIMARY KEY (" (quoted (:via-column join)) ", " (quoted (:target-via-column join)) "), "
               "FOREIGN KEY (" (quoted (:via-column join)) ") REFERENCES " (quoted (:table owner))
               " (" (quoted (get-in owner [:fields (:identity owner) :column])) "), "
               "FOREIGN KEY (" (quoted (:target-via-column join)) ") REFERENCES " (quoted (:table target))
               " (" (quoted (get-in target [:fields (:identity target) :column])) "))")])
       (reduce (fn [m [table statement]] (if (contains? m table) m (assoc m table statement)))
               {})
       vals
       vec))

(defn- linked-source [target relation]
  (let [join (:join relation)]
    (str " FROM " (quoted (:table target)) " t"
         " JOIN " (quoted (:table join)) " j"
         " ON j." (quoted (:target-via-column join))
         " = t." (quoted (get-in target [:fields (:identity target) :column]))
         " WHERE j." (quoted (:via-column join)) " = ?")))

(defn select-linked [target relation owner-id {:keys [order limit offset]}]
  (into [(str "SELECT " (str/join ", " (map #(str "t." (quoted %)) (columns-of target)))
              (linked-source target relation)
              (order-clause target order "t.")
              (when limit " LIMIT ?")
              (when offset " OFFSET ?"))]
        (concat [owner-id] (when limit [limit]) (when offset [offset]))))

(defn count-linked [target relation owner-id]
  [(str "SELECT COUNT(*) AS \"total\"" (linked-source target relation)) owner-id])

(defn select-join [relation owner-ids]
  (let [join (:join relation)
        ids  (vec (sort-by str owner-ids))]
    (into [(str "SELECT " (quoted (:via-column join)) ", " (quoted (:target-via-column join))
                " FROM " (quoted (:table join))
                (if (seq ids)
                  (str " WHERE " (quoted (:via-column join))
                       " IN (" (str/join ", " (repeat (count ids) "?")) ")")
                  " WHERE 1 = 0"))]
          ids)))

(defn insert-join [relation owner-id target-id]
  (let [join (:join relation)]
    [(str "INSERT INTO " (quoted (:table join))
          " (" (quoted (:via-column join)) ", " (quoted (:target-via-column join)) ")"
          " VALUES (?, ?)")
     owner-id target-id]))

(defn delete-join [relation owner-id target-ids]
  (let [join (:join relation)
        ids  (when target-ids (vec (sort-by str target-ids)))]
    (into [(str "DELETE FROM " (quoted (:table join))
                " WHERE " (quoted (:via-column join)) " = ?"
                (when ids
                  (if (seq ids)
                    (str " AND " (quoted (:target-via-column join))
                         " IN (" (str/join ", " (repeat (count ids) "?")) ")")
                    " AND 1 = 0")))]
          (cons owner-id ids))))
