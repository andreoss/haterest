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
   :h2       (assoc ansi-types :uuid "UUID")
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

(defn- columns-of [resource]
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

(defn ddl [model dialect]
  (mapv #(create-table model dialect (get-in model [:resources %])) (creation-order model)))

(defn- projection [resource]
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

(defn- order-clause [resource order]
  (when (seq order)
    (check-fields resource (map first order))
    (str " ORDER BY "
         (str/join ", " (for [[field direction] order]
                          (str (quoted (get-in resource [:fields field :column]))
                               (if (= :desc direction) " DESC" " ASC")))))))

(defn select [resource {:keys [where order limit offset]}]
  (check-fields resource (keys where))
  (let [predicates (for [[field _] where]
                     (str (quoted (get-in resource [:fields field :column])) " = ?"))]
    (into [(str (projection resource)
                (when (seq predicates) (str " WHERE " (str/join " AND " predicates)))
                (order-clause resource order)
                (when limit " LIMIT ?")
                (when offset " OFFSET ?"))]
          (concat (vals where) (when limit [limit]) (when offset [offset])))))

(defn count-of [resource {:keys [where]}]
  (check-fields resource (keys where))
  (let [predicates (for [[field _] where]
                     (str (quoted (get-in resource [:fields field :column])) " = ?"))]
    (into [(str "SELECT COUNT(*) AS \"total\" FROM " (quoted (:table resource))
                (when (seq predicates) (str " WHERE " (str/join " AND " predicates))))]
          (vals where))))
