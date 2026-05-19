(ns hypermedia.schema
  (:require [clojure.string :as str]
            [malli.core :as m]
            [malli.error :as me])
  (:import (java.net URLDecoder)
           (java.nio.charset StandardCharsets)
           (java.time Instant LocalDate)
           (java.util UUID)))

(def field-types
  #{:uuid :string :text :long :double :decimal :boolean :instant :date})

(def relation-kinds #{:belongs-to :has-one :has-many :many-to-many})

(def reserved-column :row_version)

(def set-kinds #{:has-many :many-to-many})

(def Field
  [:map
   [:type :keyword]
   [:generated {:optional true} :boolean]
   [:identity {:optional true} :boolean]
   [:required {:optional true} :boolean]
   [:column {:optional true} :keyword]])

(def Relation
  [:map
   [:kind :keyword]
   [:embed {:optional true} :boolean]
   [:target :keyword]
   [:via :keyword]
   [:through {:optional true} :keyword]
   [:target-via {:optional true} :keyword]])

(def Projection
  [:map
   [:fields [:vector :keyword]]
   [:embed {:optional true} [:vector :keyword]]])

(def Search
  [:map
   [:predicates [:vector :keyword]]])

(def Resource
  [:map
   [:projections {:optional true} [:map-of :keyword Projection]]
   [:searches {:optional true} [:map-of :keyword Search]]
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
                :generated? (boolean (:generated spec))
                :required? (boolean (or (:required spec) (= k id))))
         (dissoc :identity :required :generated))])

(defn- normalise-relation [path id [k spec]]
  (let [self (str path "/{" (name id) "}/" (name k))]
    [k (-> spec
           (assoc :name k
                  :path self
                  :member-template (when (contains? set-kinds (:kind spec))
                                     (str self "/{" (name (:target spec)) "-key}"))
                  :join (when (= :many-to-many (:kind spec))
                          {:table             (:through spec)
                           :via-column        (column (:via spec))
                           :target-via-column (column (:target-via spec))})
                  :embed? (boolean (:embed spec))
                  :rel k)
           (dissoc :embed))]))

(defn- normalise-projection [[k spec]]
  [k {:name   k
      :fields (vec (:fields spec))
      :embed  (vec (:embed spec))}])

(defn- normalise-search [path [k spec]]
  [k {:name       k
      :predicates (vec (:predicates spec))
      :path       (str path "/search/" (name k))
      :template   (str path "/search/" (name k)
                       "{?" (str/join "," (map name (:predicates spec))) ",page,size,sort}")}])

(defn- normalise-resource [[k spec]]
  (let [collection (or (:collection spec) (plural k))
        path       (str "/" (name collection))
        id         (identity-field (:fields spec))
        fields     (into {} (map (partial normalise-field id)) (:fields spec))]
    [k {:name          k
        :collection    collection
        :path          path
        :self-template (str path "/{" (name (or id :id)) "}")
        :profile-path  (str "/profile/" (name collection))
        :search-path   (str path "/search")
        :projections   (into {} (map normalise-projection) (:projections spec))
        :searches      (into {} (map (partial normalise-search path)) (:searches spec))
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
           [f spec'] (:fields spec)
           :when (= reserved-column (or (:column spec') (column f)))]
       {:path [:resources k :fields f] :error :reserved-column :detail reserved-column})
     (for [[k spec] resources
           [r spec'] (:relations spec)
           :when (not (contains? relation-kinds (:kind spec')))]
       {:path [:resources k :relations r] :error :unknown-kind :detail (:kind spec')})
     (for [[k spec] resources
           [r spec'] (:relations spec)
           :when (not (contains? names (:target spec')))]
       {:path [:resources k :relations r] :error :unknown-target :detail (:target spec')})
     (for [[k spec] resources
           [r relation] (:relations spec)
           :when (and (= :many-to-many (:kind relation))
                      (or (nil? (:through relation)) (nil? (:target-via relation))))]
       {:path [:resources k :relations r] :error :join-not-declared})
     (for [[k spec] resources
           [r relation] (:relations spec)
           :when (and (not= :many-to-many (:kind relation)) (:through relation))]
       {:path [:resources k :relations r] :error :join-not-allowed})
     (for [[k spec] resources
           [r relation] (:relations spec)
           :when (and (= :many-to-many (:kind relation))
                      (= (:via relation) (:target-via relation)))]
       {:path [:resources k :relations r] :error :join-columns-clash})
     (let [tables (into #{} (map (fn [[k spec]] (or (:table spec) (:collection spec) (plural k))))
                        resources)]
       (for [[k spec] resources
             [r relation] (:relations spec)
             :when (and (= :many-to-many (:kind relation))
                        (contains? tables (:through relation)))]
         {:path [:resources k :relations r] :error :join-table-taken}))
     (for [[k spec] resources
           [r relation] (:relations spec)
           :when (and (contains? #{:belongs-to :has-one} (:kind relation))
                      (not (contains? (:fields spec) (:via relation))))]
       {:path [:resources k :relations r] :error :via-not-a-field})
     (for [[k spec] resources
           [r relation] (:relations spec)
           :when (and (= :has-many (:kind relation))
                      (contains? resources (:target relation))
                      (not (contains? (:fields (get resources (:target relation))) (:via relation))))]
       {:path [:resources k :relations r] :error :via-not-a-field-of-the-target})
     (for [[k spec] resources
           [p projection] (:projections spec)
           f (:fields projection)
           :when (not (contains? (:fields spec) f))]
       {:path [:resources k :projections p] :error :unknown-field :detail f})
     (for [[k spec] resources
           [p projection] (:projections spec)
           r (:embed projection)
           :when (not (contains? (:relations spec) r))]
       {:path [:resources k :projections p] :error :unknown-relation :detail r})
     (for [[k spec] resources
           [s search] (:searches spec)
           f (:predicates search)
           :when (not (contains? (:fields spec) f))]
       {:path [:resources k :searches s] :error :unknown-field :detail f})
     (for [[k spec] resources
           [f spec'] (:fields spec)
           :when (and (:generated spec')
                      (or (not (:identity spec')) (not= :uuid (:type spec'))))]
       {:path [:resources k :fields f] :error :cannot-generate})
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
    {:curie     (if (contains? config :curie) (:curie config) "rel")
     :order     (vec (sort (keys (:resources config))))
     :resources (into {} (map normalise-resource) (:resources config))}))

(defn coerce [type value]
  (when (some? value)
    (try
      (case type
        (:string :text) (str value)
        :long           (if (integer? value) (long value) (Long/parseLong (str value)))
        :double         (if (number? value) (double value) (Double/parseDouble (str value)))
        :decimal        (bigdec value)
        :boolean        (if (boolean? value) value (Boolean/parseBoolean (str value)))
        :uuid           (if (uuid? value) value (UUID/fromString (str value)))
        :instant        (if (instance? Instant value) value (Instant/parse (str value)))
        :date           (if (instance? LocalDate value) value (LocalDate/parse (str value)))
        nil)
      (catch Exception _ nil))))

(defn- href-of [value]
  (cond
    (string? value) value
    (map? value)    (or (:href value) (get value "href"))))

(defn- last-segment [href]
  (when-let [segment (some-> href (str/split #"/") last not-empty)]
    (URLDecoder/decode segment StandardCharsets/UTF_8)))

(defn- conform-entry [resource [k value]]
  (let [field    (get-in resource [:fields k])
        relation (get-in resource [:relations k])]
    (cond
      field
      (let [coerced (coerce (:type field) value)]
        (cond
          (some? coerced) {:entry [k coerced]}
          (nil? value)    {:entry [k nil]}
          :else           {:error {:field k :error :unreadable}}))

      (= :belongs-to (:kind relation))
      (let [target  (get-in resource [:fields (:via relation)])
            coerced (coerce (:type target) (last-segment (href-of value)))]
        (if (some? coerced)
          {:entry [(:via relation) coerced]}
          {:error {:field k :error :unreadable}}))

      :else {:error {:field k :error :unknown}})))

(defn conform [resource row {:keys [partial? identity]}]
  (let [outcomes (mapv #(conform-entry resource %) row)
        given    (into {} (keep :entry) outcomes)
        id       (:identity resource)
        clash    (when (and identity (contains? given id) (not= (get given id) identity))
                   {:field id :error :conflict})
        value    (cond
                   identity (assoc given id identity)
                   (and (not partial?)
                        (not (contains? given id))
                        (get-in resource [:fields id :generated?]))
                   (assoc given id (random-uuid))
                   :else given)
        missing  (when-not partial?
                   (for [k (:field-order resource)
                         :when (and (get-in resource [:fields k :required?])
                                    (not (contains? value k)))]
                     {:field k :error :missing}))]
    {:value  value
     :errors (into (vec (keep :error outcomes)) (concat (when clash [clash]) missing))}))
