(ns hypermedia.store.jdbc
  (:require [hypermedia.sql :as sql]
            [hypermedia.store :as store]
            [next.jdbc :as jdbc]
            [next.jdbc.connection :as connection]
            [next.jdbc.result-set :as rs])
  (:import (com.zaxxer.hikari HikariDataSource)))

(def ^:private options {:builder-fn rs/as-unqualified-kebab-maps})

(defn- type-of [resource field]
  (get-in resource [:fields field :type]))

(defn- encode-row [dialect resource row]
  (reduce-kv (fn [m k v] (assoc m k (sql/encode dialect (type-of resource k) v))) {} row))

(defn- decode-row [dialect resource row]
  (when row
    (reduce-kv (fn [m k v]
                 (assoc m k (if (contains? (:fields resource) k)
                              (sql/decode dialect (type-of resource k) v)
                              v)))
               {} row)))

(defn- encode-criteria [dialect resource criteria]
  (cond-> criteria
    (:where criteria) (update :where #(encode-row dialect resource %))))

(defrecord Jdbc [datasource model dialect statements]
  store/Store
  (fetch [_ resource id]
    (->> [(or (get-in statements [(:name resource) :by-identity]) (sql/select-by-identity resource))
          (sql/encode dialect (type-of resource (:identity resource)) id)]
         (#(jdbc/execute-one! datasource % options))
         (decode-row dialect resource)))
  (query [_ resource criteria]
    (mapv #(decode-row dialect resource %)
          (jdbc/execute! datasource (sql/select resource (encode-criteria dialect resource criteria)) options)))
  (total [_ resource criteria]
    (:total (jdbc/execute-one! datasource
                               (sql/count-of resource (encode-criteria dialect resource criteria))
                               options)))
  (probe [_]
    (boolean (try (jdbc/execute-one! datasource ["SELECT 1"]) true
                  (catch Exception _ false))))
  (create! [this resource row]
    (jdbc/execute! datasource (sql/insert resource (encode-row dialect resource row)))
    (store/fetch this resource (get row (:identity resource))))
  (replace! [this resource id row]
    (let [existed (some? (store/fetch this resource id))
          blank   (zipmap (:field-order resource) (repeat nil))
          stored  (assoc (merge blank row) (:identity resource) id)]
      (if existed
        (jdbc/execute! datasource (sql/update-by-identity resource
                                                          (sql/encode dialect (type-of resource (:identity resource)) id)
                                                          (encode-row dialect resource stored)))
        (jdbc/execute! datasource (sql/insert resource (encode-row dialect resource stored))))
      {:created? (not existed) :row (store/fetch this resource id)}))
  (amend! [this resource id row]
    (when (store/fetch this resource id)
      (let [changes (dissoc row (:identity resource))]
        (when (seq changes)
          (jdbc/execute! datasource
                         (sql/update-by-identity resource
                                                 (sql/encode dialect (type-of resource (:identity resource)) id)
                                                 (encode-row dialect resource changes)))))
      (store/fetch this resource id)))
  (erase! [this resource id]
    (if (store/fetch this resource id)
      (do (jdbc/execute! datasource
                         (sql/delete-by-identity resource
                                                 (sql/encode dialect (type-of resource (:identity resource)) id)))
          true)
      false))
  (transact [this body]
    (jdbc/with-transaction [tx datasource]
      (body (assoc this :datasource tx)))))

(defn migrate! [datasource model dialect]
  (run! #(jdbc/execute! datasource [%]) (sql/ddl model dialect)))

(defn open [{:keys [url model migrate? statements]}]
  (let [dialect    (sql/dialect url)
        datasource (connection/->pool HikariDataSource
                                      (cond-> {:jdbcUrl url}
                                        (= :sqlite dialect) (assoc :maximumPoolSize 1)))]
    (when migrate? (migrate! datasource model dialect))
    (->Jdbc datasource model dialect (or statements (sql/statements model)))))

(defn close [opened]
  (.close ^HikariDataSource (:datasource opened)))
