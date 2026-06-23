(ns hypermedia.store.jdbc
  (:require [clojure.string :as str]
            [hypermedia.evolve :as evolve]
            [hypermedia.sql :as sql]
            [hypermedia.store :as store]
            [next.jdbc :as jdbc]
            [next.jdbc.connection :as connection]
            [next.jdbc.result-set :as rs])
  (:import (com.zaxxer.hikari HikariDataSource)))

(def ^:private options {:builder-fn rs/as-unqualified-kebab-maps})

(defn- changed [result]
  (or (:next.jdbc/update-count (first result)) 0))

(def ^:private conflict-codes #{1020 1213 1205 90131})

(defn- conflict? [^Exception exception]
  (loop [cause exception]
    (cond
      (nil? cause) false
      (instance? java.sql.SQLException cause)
      (let [^java.sql.SQLException e cause]
        (or (str/starts-with? (str (.getSQLState e)) "40")
            (contains? conflict-codes (.getErrorCode e))
            (recur (.getCause e))))
      :else (recur (.getCause cause)))))

(defn- attempt [body]
  (try (body)
       (catch Exception e
         (if (conflict? e) {:outcome :conflict} (throw e)))))

(extend-protocol rs/ReadableColumn
  java.sql.Clob
  (read-column-by-label [value _]
    (.getSubString ^java.sql.Clob value 1 (int (.length ^java.sql.Clob value))))
  (read-column-by-index [value _ _]
    (.getSubString ^java.sql.Clob value 1 (int (.length ^java.sql.Clob value)))))

(defn- type-of [resource field]
  (get-in resource [:fields field :type]))

(defn- encode-row [dialect resource row]
  (reduce-kv (fn [m k v] (assoc m k (sql/encode dialect (type-of resource k) v))) {} row))

(def ^:private version-alias
  (keyword (str/replace (name sql/version-column) \_ \-)))

(defn- decode-row [dialect resource row]
  (when row
    (reduce-kv (fn [m k v]
                 (cond
                   (= k version-alias)             (assoc m sql/version-key v)
                   (contains? (:fields resource) k) (assoc m k (sql/decode dialect (type-of resource k) v))
                   :else                            (assoc m k v)))
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
          (jdbc/execute! datasource
                         (sql/select dialect resource (encode-criteria dialect resource criteria))
                         options)))
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
  (replace! [this resource id row expected]
    (let [key    (sql/encode dialect (type-of resource (:identity resource)) id)
          blank  (zipmap (:field-order resource) (repeat nil))
          stored (assoc (merge blank row) (:identity resource) id)]
      (attempt
       (fn []
         (if (pos? (changed (jdbc/execute! datasource
                                           (sql/update-by-identity resource key
                                                                   (encode-row dialect resource stored)
                                                                   expected))))
           {:outcome :written :row (store/fetch this resource id)}
           (if (nil? (store/fetch this resource id))
             (do (jdbc/execute! datasource (sql/insert resource (encode-row dialect resource stored)))
                 {:outcome :created :row (store/fetch this resource id)})
             {:outcome :stale}))))))
  (amend! [this resource id row expected]
    (let [key     (sql/encode dialect (type-of resource (:identity resource)) id)
          changes (dissoc row (:identity resource))]
      (attempt
       (fn []
         (if (pos? (changed (jdbc/execute! datasource
                                           (sql/update-by-identity resource key
                                                                   (encode-row dialect resource changes)
                                                                   expected))))
           {:outcome :written :row (store/fetch this resource id)}
           (if (nil? (store/fetch this resource id))
             {:outcome :absent}
             {:outcome :stale}))))))
  (erase! [this resource id expected]
    (let [key (sql/encode dialect (type-of resource (:identity resource)) id)]
      (attempt
       (fn []
         (if (pos? (changed (jdbc/execute! datasource (sql/delete-by-identity resource key expected))))
           {:outcome :erased}
           (if (nil? (store/fetch this resource id))
             {:outcome :absent}
             {:outcome :stale}))))))
  (amend-where! [_ resource where row]
    (changed (jdbc/execute! datasource
                            (sql/update-where resource
                                              (encode-row dialect resource where)
                                              (encode-row dialect resource row)))))
  (transact [this body]
    (jdbc/with-transaction [tx datasource]
      (body (assoc this :datasource tx))))
  (linked [_ owner target relation owner-id criteria]
    (mapv #(decode-row dialect target %)
          (jdbc/execute! datasource
                         (sql/select-linked dialect target relation
                                            (sql/encode dialect (type-of owner (:identity owner)) owner-id)
                                            criteria)
                         options)))
  (linked-total [_ owner target relation owner-id]
    (:total (jdbc/execute-one! datasource
                               (sql/count-linked target relation
                                                 (sql/encode dialect (type-of owner (:identity owner)) owner-id))
                               options)))
  (links-of [_ owner target relation owner-ids]
    (let [join       (:join relation)
          owner-key  (keyword (str/replace (name (:via-column join)) \_ \-))
          target-key (keyword (str/replace (name (:target-via-column join)) \_ \-))
          owner-type (type-of owner (:identity owner))
          other-type (type-of target (:identity target))]
      (mapv (fn [row] [(sql/decode dialect owner-type (get row owner-key))
                       (sql/decode dialect other-type (get row target-key))])
            (jdbc/execute! datasource
                           (sql/select-join relation (map #(sql/encode dialect owner-type %) owner-ids))
                           options))))
  (link! [_ owner target relation owner-id target-ids]
    (let [key    (sql/encode dialect (type-of owner (:identity owner)) owner-id)
          groups (mapv (fn [target-id]
                         [key (sql/encode dialect (type-of target (:identity target)) target-id)])
                       target-ids)]
      (when (seq groups)
        (jdbc/execute-batch! datasource (first (sql/insert-join relation nil nil)) groups {}))
      (count target-ids)))
  (unlink! [_ owner target relation owner-id target-ids]
    (let [key (sql/encode dialect (type-of owner (:identity owner)) owner-id)
          ids (when target-ids
                (map #(sql/encode dialect (type-of target (:identity target)) %) target-ids))]
      (or (:next.jdbc/update-count
           (first (jdbc/execute! datasource (sql/delete-join relation key ids))))
          0))))

(def ^:private attempts 6)

(defn- attempted [datasource statements]
  (reduce (fn [failed statement]
            (try (jdbc/execute! datasource [statement]) failed
                 (catch Exception e
                   (conj failed {:statement statement
                                 :message (first (str/split-lines (str (.getMessage e))))}))))
          []
          statements))

(defn- pause [attempt]
  (Thread/sleep (long (+ 40 (* attempt 120) (rand-int 160)))))

(defn evolution [datasource model dialect]
  (evolve/evolution datasource model dialect))

(defn migrate! [datasource model dialect]
  (loop [attempt 0 failures []]
    (let [{:keys [statements refusals notes]} (evolve/evolution datasource model dialect)]
      (cond
        (seq refusals)
        (throw (ex-info "the store cannot be evolved to this schema"
                        {:type ::refused :refusals refusals}))

        (empty? statements)
        notes

        (>= attempt attempts)
        (throw (ex-info "the store could not be evolved to this schema"
                        {:type ::unfinished :failures failures :remaining statements}))

        :else
        (let [failed (attempted datasource statements)]
          (when (seq failed) (pause attempt))
          (recur (inc attempt) failed))))))

(defn open [{:keys [url model migrate? statements pool]}]
  (let [dialect    (sql/dialect url)
        datasource (connection/->pool HikariDataSource
                                      (cond-> {:jdbcUrl url}
                                        (= :sqlite dialect) (assoc :maximumPoolSize 1)
                                        (sql/session-setup dialect)
                                        (assoc :connectionInitSql (sql/session-setup dialect))
                                        (:size pool)     (assoc :maximumPoolSize (:size pool))
                                        (:idle pool)     (assoc :minimumIdle (:idle pool))
                                        (:timeout pool)  (assoc :connectionTimeout (:timeout pool))))]
    (when migrate? (migrate! datasource model dialect))
    (->Jdbc datasource model dialect (or statements (sql/statements model)))))

(defn close [opened]
  (.close ^HikariDataSource (:datasource opened)))
