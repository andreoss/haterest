(ns hypermedia.sql
  (:require [clojure.string :as str])
  (:import (java.time Instant LocalDate LocalDateTime ZoneOffset)))

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
   :hsqldb   (assoc ansi-types :uuid "UUID" :instant "TIMESTAMP WITH TIME ZONE")
   :derby    (assoc ansi-types :double "DOUBLE")
   :mysql    (assoc ansi-types :text "TEXT" :instant "DATETIME")
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
    "jdbc:hsqldb:"     :hsqldb
    "jdbc:derby:"      :derby
    "jdbc:mysql:"      :mysql
    "jdbc:mariadb:"    :mysql
    :ansi))

(def ^:private without-if-not-exists #{:derby})

(defn creates-only-once? [dialect]
  (contains? without-if-not-exists dialect))

(defn session-setup [dialect]
  (when (= :mysql dialect) "SET SESSION sql_mode='ANSI_QUOTES'"))

(defn- fetch-clause [dialect limit offset]
  (if (= :derby dialect)
    {:sql    (str (when offset " OFFSET ? ROWS")
                  (when limit " FETCH NEXT ? ROWS ONLY"))
     :params (into (if offset [offset] []) (if limit [limit] []))}
    {:sql    (str (when limit " LIMIT ?") (when offset " OFFSET ?"))
     :params (into (if limit [limit] []) (if offset [offset] []))}))

(defn column-type [dialect type]
  (or (get-in dialect-types [dialect type])
      (get ansi-types type)
      (throw (ex-info "unknown type" {:type ::unknown-type :dialect dialect :field-type type}))))

(def version-column :row_version)

(def version-key :hypermedia/version)

(defn quoted [identifier]
  (str \" (str/replace (name identifier) \" \_) \"))

(defn columns-of [resource]
  (mapv #(get-in resource [:fields % :column]) (:field-order resource)))

(defn column-clause [dialect resource field]
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
                      [(str (quoted version-column) " " (column-type dialect :long) " DEFAULT 0 NOT NULL")]
                      [(str "PRIMARY KEY (" identity-column ")")]
                      (foreign-keys model resource))]
    (str "CREATE TABLE " (when-not (creates-only-once? dialect) "IF NOT EXISTS ")
         (quoted (:table resource))
         " (" (str/join ", " parts) ")")))

(declare join-tables)

(declare indexes)
(declare join-tables)

(defn tables [model dialect]
  (into (mapv #(create-table model dialect (get-in model [:resources %])) (creation-order model))
        (join-tables model dialect)))

(defn ddl [model dialect]
  (into (tables model dialect) (indexes model dialect)))

(defn projection [resource]
  (str "SELECT " (str/join ", " (map quoted (conj (columns-of resource) version-column)))
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

(defn select
  ([resource criteria] (select :ansi resource criteria))
  ([dialect resource {:keys [where order limit offset]}]
   (check-fields resource (keys where))
   (let [clause (where-clause resource where)
         fetch  (fetch-clause dialect limit offset)]
     (into [(str (projection resource)
                 (:sql clause)
                 (order-clause resource order)
                 (:sql fetch))]
           (concat (:params clause) (:params fetch))))))

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
                " (" (str/join ", " (conj (mapv #(quoted (get-in resource [:fields % :column])) fields)
                                          (quoted version-column))) ")"
                " VALUES (" (str/join ", " (repeat (inc (count fields)) "?")) ")")]
          (conj (mapv #(get row %) fields) 0))))

(defn update-by-identity
  ([resource id row] (update-by-identity resource id row nil))
  ([resource id row expected]
   (check-fields resource (keys row))
   (let [fields (remove #(= % (:identity resource)) (write-columns resource row))
         bump   (str (quoted version-column) " = " (quoted version-column) " + 1")]
     (into [(str "UPDATE " (quoted (:table resource)) " SET "
                 (str/join ", " (conj (mapv #(str (quoted (get-in resource [:fields % :column])) " = ?")
                                            fields)
                                      bump))
                 " WHERE " (quoted (get-in resource [:fields (:identity resource) :column])) " = ?"
                 (when expected (str " AND " (quoted version-column) " = ?")))]
           (cond-> (conj (mapv #(get row %) fields) id)
             expected (conj expected))))))

(defn delete-by-identity
  ([resource id] (delete-by-identity resource id nil))
  ([resource id expected]
   (cond-> [(str "DELETE FROM " (quoted (:table resource))
                 " WHERE " (quoted (get-in resource [:fields (:identity resource) :column])) " = ?"
                 (when expected (str " AND " (quoted version-column) " = ?")))
            id]
     expected (conj expected))))

(defn- stored-as [dialect type]
  (column-type dialect type))

(defn- textual? [dialect type]
  (let [storage (stored-as dialect type)]
    (or (str/starts-with? storage "TEXT")
        (str/starts-with? storage "VARCHAR")
        (str/starts-with? storage "CLOB")
        (str/starts-with? storage "CHAR"))))

(defn- numeric? [dialect type]
  (let [storage (stored-as dialect type)]
    (or (str/starts-with? storage "INT")
        (str/starts-with? storage "BIGINT")
        (str/starts-with? storage "NUMERIC")
        (str/starts-with? storage "SMALLINT"))))

(def ^:private readers
  {:uuid    (fn [v] (if (uuid? v) v (java.util.UUID/fromString (str v))))
   :boolean (fn [v] (cond (boolean? v) v
                          (number? v)  (not (zero? (long v)))
                          :else        (Boolean/parseBoolean (str v))))
   :instant (fn [v] (condp instance? v
                      java.time.Instant        v
                      java.time.OffsetDateTime (.toInstant ^java.time.OffsetDateTime v)
                      java.sql.Timestamp       (.toInstant (.atOffset (.toLocalDateTime ^java.sql.Timestamp v)
                                                            ZoneOffset/UTC))
                      (java.time.Instant/parse (str v))))
   :date    (fn [v] (condp instance? v
                      java.time.LocalDate v
                      java.sql.Date       (.toLocalDate ^java.sql.Date v)
                      (java.time.LocalDate/parse (str v))))
   :decimal (fn [v] (bigdec v))
   :text    (fn [v] (if (instance? java.sql.Clob v)
                      (.getSubString ^java.sql.Clob v 1 (int (.length ^java.sql.Clob v)))
                      (str v)))
   :string  (fn [v] (if (instance? java.sql.Clob v)
                      (.getSubString ^java.sql.Clob v 1 (int (.length ^java.sql.Clob v)))
                      v))})

(defn encode [dialect type value]
  (cond
    (nil? value)  value
    (coll? value) (mapv #(encode dialect type %) value)
    :else
    (case type
      :uuid    (if (textual? dialect :uuid) (str value) value)
      :boolean (cond (numeric? dialect :boolean) (if value 1 0)
                     (textual? dialect :boolean) (str (boolean value))
                     :else value)
      :instant (cond
                 (textual? dialect :instant)      (str value)
                 (not (instance? Instant value))  value
                 (str/includes? (stored-as dialect :instant) "WITH TIME ZONE")
                 (.atOffset ^Instant value ZoneOffset/UTC)
                 :else (java.sql.Timestamp/valueOf (LocalDateTime/ofInstant ^Instant value ZoneOffset/UTC)))
      :date    (cond
                 (textual? dialect :date)           (str value)
                 (instance? LocalDate value)        (java.sql.Date/valueOf ^LocalDate value)
                 :else                              value)
      value)))

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

(defn join-table-statements [model dialect]
  (->> (for [k (:order model)
             :let [owner (get-in model [:resources k])]
             [_ relation] (:relations owner)
             :when (= :many-to-many (:kind relation))
             :let [join   (:join relation)
                   target (get-in model [:resources (:target relation)])]]
         [(:table join)
          (str "CREATE TABLE " (when-not (creates-only-once? dialect) "IF NOT EXISTS ")
               (quoted (:table join))
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
               {})))

(defn join-tables [model dialect]
  (vec (vals (join-table-statements model dialect))))

(defn- linked-source [target relation]
  (let [join (:join relation)]
    (str " FROM " (quoted (:table target)) " t"
         " JOIN " (quoted (:table join)) " j"
         " ON j." (quoted (:target-via-column join))
         " = t." (quoted (get-in target [:fields (:identity target) :column]))
         " WHERE j." (quoted (:via-column join)) " = ?")))

(defn select-linked
  ([target relation owner-id criteria] (select-linked :ansi target relation owner-id criteria))
  ([dialect target relation owner-id {:keys [order limit offset]}]
   (let [fetch (fetch-clause dialect limit offset)]
     (into [(str "SELECT " (str/join ", " (map #(str "t." (quoted %))
                                               (conj (columns-of target) version-column)))
                 (linked-source target relation)
                 (order-clause target order "t.")
                 (:sql fetch))]
           (concat [owner-id] (:params fetch))))))

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

(defn update-where [resource where row]
  (check-fields resource (keys row))
  (check-fields resource (keys where))
  (let [fields (remove #(= % (:identity resource)) (write-columns resource row))
        bump   (str (quoted version-column) " = " (quoted version-column) " + 1")
        clause (where-clause resource where)]
    (into [(str "UPDATE " (quoted (:table resource)) " SET "
                (str/join ", " (conj (mapv #(str (quoted (get-in resource [:fields % :column])) " = ?")
                                           fields)
                                     bump))
                (:sql clause))]
          (concat (map #(get row %) fields) (:params clause)))))

(defn- column-of [resource field]
  (get-in resource [:fields field :column]))

(def ^:private indexes-its-foreign-keys #{:h2 :mysql :derby :hsqldb})

(defn indexes-foreign-keys? [dialect]
  (contains? indexes-its-foreign-keys dialect))

(defn indexed-columns [model dialect]
  (reduce
   (fn [acc k]
     (let [resource (get-in model [:resources k])
           table    (:table resource)
           identity (column-of resource (:identity resource))
           mine     (concat
                     (for [f (:field-order resource)
                           :when (get-in resource [:fields f :indexed?])]
                       [table (column-of resource f)])
                     (when-not (indexes-foreign-keys? dialect)
                       (for [[_ relation] (:relations resource)
                             :when (= :belongs-to (:kind relation))]
                         [table (column-of resource (:via relation))]))
                     (for [[_ search] (:searches resource)
                           f (:predicates search)]
                       [table (column-of resource f)])
                     (when-not (indexes-foreign-keys? dialect)
                       (for [[_ relation] (:relations resource)
                             :when (= :has-many (:kind relation))
                             :let [target (get-in model [:resources (:target relation)])]
                             :when target]
                         [(:table target) (column-of target (:via relation))]))
                     (for [[_ relation] (:relations resource)
                           :when (= :many-to-many (:kind relation))]
                       [(get-in relation [:join :table])
                        (get-in relation [:join :target-via-column])]))]
       (reduce (fn [m [t c]]
                 (if (and c (not (and (= t table) (= c identity))))
                   (update m t (fnil conj (sorted-set)) c)
                   m))
               acc mine)))
   {}
   (:order model)))

(defn index-of [dialect table column]
  (str "CREATE INDEX " (when-not (creates-only-once? dialect) "IF NOT EXISTS ")
       (quoted (str "ix_" (name table) "_" (name column)))
       " ON " (quoted table) " (" (quoted column) ")"))

(defn indexes [model dialect]
  (vec (for [[table columns] (sort-by key (indexed-columns model dialect))
             column columns]
         (index-of dialect table column))))

(defn add-column [dialect resource field]
  (str "ALTER TABLE " (quoted (:table resource))
       " ADD COLUMN " (column-clause dialect resource field)))

(defn add-version-column [dialect table]
  (str "ALTER TABLE " (quoted table)
       " ADD COLUMN " (quoted version-column) " " (column-type dialect :long) " DEFAULT 0 NOT NULL"))

(def ^:private adds-required-columns #{:h2 :hsqldb :derby :postgres :mysql :ansi})

(defn adds-required-column? [dialect]
  (contains? adds-required-columns dialect))

(defn table-of [model table]
  (some (fn [k] (let [resource (get-in model [:resources k])]
                  (when (= table (:table resource)) resource)))
        (:order model)))

(defn creation-of [model dialect table]
  (if-let [resource (table-of model table)]
    (create-table model dialect resource)
    (get (join-table-statements model dialect) table)))
