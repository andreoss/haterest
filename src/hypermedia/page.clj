(ns hypermedia.page
  (:require [clojure.string :as str]
            [hypermedia.hal :as hal]
            [hypermedia.uri :as uri]))

(def default-size 20)
(def maximum-size 200)

(defn- as-sequence [value]
  (cond (nil? value) [] (sequential? value) (vec value) :else [value]))

(defn- whole [value fallback floor]
  (if (nil? value)
    {:value fallback}
    (if-let [parsed (try (Long/parseLong (str value)) (catch Exception _ nil))]
      (if (< parsed floor)
        {:error {:field :slice :error :out-of-range :detail (str value)}}
        {:value parsed})
      {:error {:field :slice :error :unreadable :detail (str value)}})))

(defn- clause [resource text]
  (let [[field direction] (str/split (str text) #",")
        field'    (keyword field)
        direction (or (some-> direction str/lower-case) "asc")]
    (cond
      (not (contains? (:fields resource) field'))
      {:error {:field :sort :error :unknown :detail field}}

      (not (contains? #{"asc" "desc"} direction))
      {:error {:field :sort :error :unreadable :detail direction}}

      :else {:value [field' (keyword direction)]})))

(defn parse [resource query-params]
  (let [number  (whole (get query-params "page") 0 0)
        size    (whole (get query-params "size") default-size 1)
        clauses (mapv #(clause resource %) (as-sequence (get query-params "sort")))]
    {:number (or (:value number) 0)
     :size   (min (or (:value size) default-size) maximum-size)
     :sort   (mapv :value (filter :value clauses))
     :errors (into (vec (keep :error [number size])) (keep :error clauses))}))

(defn criteria [pageable where]
  (cond-> {:where where}
    (seq (:sort pageable)) (assoc :order (:sort pageable))
    true (assoc :limit (:size pageable)
                :offset (* (:number pageable) (:size pageable)))))

(defn href [base pageable number]
  (str base "?"
       (str/join "&"
                 (into [(str "page=" number) (str "size=" (:size pageable))]
                       (for [[field direction] (:sort pageable)]
                         (str "sort=" (uri/encode (str (name field) "," (name direction)))))))))

(defn pages [pageable total]
  (max 1 (long (Math/ceil (/ (double total) (:size pageable))))))

(defn descriptor [pageable total]
  {:size          (:size pageable)
   :totalElements total
   :totalPages    (pages pageable total)
   :number        (:number pageable)})

(defn links [base pageable total]
  (let [last-page (dec (pages pageable total))
        number    (:number pageable)]
    (cond-> {:self  (hal/href (href base pageable number))
             :first (hal/href (href base pageable 0))
             :last  (hal/href (href base pageable last-page))}
      (pos? number)        (assoc :prev (hal/href (href base pageable (dec number))))
      (< number last-page) (assoc :next (hal/href (href base pageable (inc number)))))))
