(ns hypermedia.schema
  (:require [clojure.string :as str]
            [malli.core :as m]
            [malli.error :as me])
  (:import (java.time Instant LocalDate)
           (java.util UUID)))

(def field-types
  #{:uuid :string :text :long :double :decimal :boolean :instant :date})

(def relation-kinds #{:belongs-to :has-one :has-many})

(def Field
  [:map
   [:type :keyword]
   [:identity {:optional true} :boolean]
   [:required {:optional true} :boolean]
   [:column {:optional true} :keyword]])

(def Relation
  [:map
   [:kind :keyword]
   [:target :keyword]
   [:via :keyword]])

(def Resource
  [:map
   [:collection {:optional true} :keyword]
   [:table {:optional true} :keyword]
   [:field-order {:optional true} [:vector :keyword]]
   [:fields [:map-of :keyword Field]]
   [:relations {:optional true} [:map-of :keyword Relation]]])

(def Config
  [:map [:resources [:map-of :keyword Resource]]])

(defn- plural [k]
  (let [s (name k)]
    (keyword
     (cond
       (re-find #"(?:s|x|z|ch|sh)$" s)  (str s "es")
       (re-find #"[^aeiou]y$" s)        (str (subs s 0 (dec (count s))) "ies")
       :else                            (str s "s")))))

(defn- column [k]
  (keyword (str/replace (name k) \- \_)))

(defn- identity-field [fields]
  (some (fn [[k v]] (when (:identity v) k)) fields))

(defn- order-of [resource]
  (let [fields (:fields resource)
        id     (identity-field fields)
        given  (or (:field-order resource) (vec (keys fields)))]
    (vec (distinct (cons id (remove nil? given))))))

(defn- normalise-field [id [k spec]]
  [k (-> spec
         (assoc :name k
                :column (or (:column spec) (column k))
                :identity? (= k id)
                :required? (boolean (or (:required spec) (= k id))))
         (dissoc :identity :required))])

(defn- normalise-relation [path id [k spec]]
  [k (assoc spec
            :name k
            :path (str path "/{" (name id) "}/" (name k))
            :rel k)])

(defn- normalise-resource [[k spec]]
  (let [collection (or (:collection spec) (plural k))
        path       (str "/" (name collection))
        id         (identity-field (:fields spec))
        fields     (into {} (map (partial normalise-field id)) (:fields spec))]
    [k {:name        k
        :collection  collection
        :path        path
        :table       (or (:table spec) collection)
        :identity    id
        :fields      fields
        :field-order (order-of spec)
        :relations   (into {} (map (partial normalise-relation path (or id :id)))
                           (:relations spec))}]))

(defn- structural-errors [config]
  (when-let [explanation (m/explain Config config)]
    (mapv (fn [[path message]] {:path path :error :malformed :detail message})
          (me/humanize explanation {:wrap (juxt :in :message)}))))

(defn- semantic-errors [config]
  (let [resources (:resources config)
        names     (set (keys resources))]
    (concat
     (for [[k spec] resources
           :when (nil? (identity-field (:fields spec)))]
       {:path [:resources k] :error :no-identity})
     (for [[k spec] resources
           [f spec'] (:fields spec)
           :when (not (contains? field-types (:type spec')))]
       {:path [:resources k :fields f] :error :unknown-type :detail (:type spec')})
     (for [[k spec] resources
           [r spec'] (:relations spec)
           :when (not (contains? relation-kinds (:kind spec')))]
       {:path [:resources k :relations r] :error :unknown-kind :detail (:kind spec')})
     (for [[k spec] resources
           [r spec'] (:relations spec)
           :when (not (contains? names (:target spec')))]
       {:path [:resources k :relations r] :error :unknown-target :detail (:target spec')})
     (for [[collection ks] (group-by #(or (:collection (val %)) (plural (key %))) resources)
           :when (< 1 (count ks))]
       {:path [:resources] :error :duplicate-collection :detail collection}))))

(defn errors [config]
  (vec (or (seq (structural-errors config))
           (seq (semantic-errors config)))))

(defn parse [config]
  (let [problems (errors config)]
    (when (seq problems)
      (throw (ex-info "invalid schema" {:type ::invalid :errors problems})))
    {:order     (vec (sort (keys (:resources config))))
     :resources (into {} (map normalise-resource) (:resources config))}))

(defn coerce [type value]
  (try
    (case type
      (:string :text) (str value)
      :long           (Long/parseLong value)
      :double         (Double/parseDouble value)
      :decimal        (bigdec value)
      :boolean        (Boolean/parseBoolean value)
      :uuid           (UUID/fromString value)
      :instant        (Instant/parse value)
      :date           (LocalDate/parse value)
      nil)
    (catch Exception _ nil)))
