(ns hypermedia.store.jdbc
  (:require [hypermedia.sql :as sql]
            [hypermedia.store :as store]
            [next.jdbc :as jdbc]
            [next.jdbc.connection :as connection]
            [next.jdbc.result-set :as rs])
  (:import (com.zaxxer.hikari HikariDataSource)))

(def ^:private options {:builder-fn rs/as-unqualified-kebab-maps})

(defrecord Jdbc [datasource model dialect]
  store/Store
  (fetch [_ resource id]
    (jdbc/execute-one! datasource [(sql/select-by-identity resource) id] options))
  (query [_ resource criteria]
    (vec (jdbc/execute! datasource (sql/select resource criteria) options)))
  (total [_ resource criteria]
    (:total (jdbc/execute-one! datasource (sql/count-of resource criteria) options)))
  (probe [_]
    (boolean (try (jdbc/execute-one! datasource ["SELECT 1"]) true
                  (catch Exception _ false)))))

(defn migrate! [datasource model dialect]
  (run! #(jdbc/execute! datasource [%]) (sql/ddl model dialect)))

(defn open [{:keys [url model migrate?]}]
  (let [dialect    (sql/dialect url)
        datasource (connection/->pool HikariDataSource
                                      (cond-> {:jdbcUrl url}
                                        (= :sqlite dialect) (assoc :maximumPoolSize 1)))]
    (when migrate? (migrate! datasource model dialect))
    (->Jdbc datasource model dialect)))

(defn close [opened]
  (.close ^HikariDataSource (:datasource opened)))
