(ns hypermedia.alps)

(def media-type "application/alps+json")

(defn- hidden [resource]
  (into #{(:identity resource)}
        (keep (fn [[_ relation]] (when (= :belongs-to (:kind relation)) (:via relation))))
        (:relations resource)))

(defn- properties [resource]
  (let [skip (hidden resource)]
    (vec (for [k (:field-order resource)
               :when (not (skip k))]
           {:name (name k)
            :type "SEMANTIC"
            :doc  {:format "TEXT"
                   :value  (str (name (get-in resource [:fields k :type]))
                                (when (get-in resource [:fields k :required?]) ", required"))}}))))

(defn- relations [model resource]
  (vec (for [[k relation] (:relations resource)
             :let [target (get-in model [:resources (:target relation)])]]
         {:name (name k)
          :type "SAFE"
          :rt   (str "/profile/" (name (:collection target)))
          :doc  {:format "TEXT" :value (name (:kind relation))}})))

(def ^:private slice
  [{:name "page" :type "SEMANTIC" :doc {:format "TEXT" :value "slice to return, from zero"}}
   {:name "size" :type "SEMANTIC" :doc {:format "TEXT" :value "rows per slice"}}
   {:name "sort" :type "SEMANTIC" :doc {:format "TEXT" :value "field,asc or field,desc"}}])

(defn descriptor [model resource]
  (let [item       (name (:name resource))
        collection (name (:collection resource))
        represents (str item "-representation")]
    {:alps
     {:version    "1.0"
      :descriptor [{:id         represents
                    :type       "SEMANTIC"
                    :descriptor (into (properties resource) (relations model resource))}
                   {:id (str "get-" collection) :name collection :type "SAFE"
                    :rt (str "#" represents) :descriptor slice}
                   {:id (str "create-" collection) :name collection :type "UNSAFE"
                    :rt (str "#" represents)}
                   {:id (str "get-" item) :name item :type "SAFE" :rt (str "#" represents)}
                   {:id (str "update-" item) :name item :type "IDEMPOTENT" :rt (str "#" represents)}
                   {:id (str "patch-" item) :name item :type "UNSAFE" :rt (str "#" represents)}
                   {:id (str "delete-" item) :name item :type "IDEMPOTENT"}]}}))
