(ns hypermedia.forms)

(def media-type "application/prs.hal-forms+json")

(def ^:private input-types
  {:string  "text"
   :text    "textarea"
   :long    "number"
   :double  "number"
   :decimal "number"
   :boolean "checkbox"
   :date    "date"
   :instant "datetime"
   :uuid    "text"})

(defn- hidden [resource]
  (into #{(:identity resource)}
        (keep (fn [[_ relation]] (when (= :belongs-to (:kind relation)) (:via relation))))
        (:relations resource)))

(defn properties [resource]
  (let [skip (hidden resource)]
    (into (vec (for [k (:field-order resource)
                     :when (not (skip k))
                     :let [field (get-in resource [:fields k])]]
                 {:name     (name k)
                  :type     (get input-types (:type field) "text")
                  :required (boolean (:required? field))}))
          (for [[k relation] (:relations resource)
                :when (= :belongs-to (:kind relation))]
            {:name (name k) :type "url" :required false}))))

(defn templates [resource kind]
  (let [item (name (:name resource))]
    (case kind
      :collection {:default {:method      "POST"
                             :title       (str "create a " item)
                             :contentType "application/json"
                             :target      (:path resource)
                             :properties  (properties resource)}}
      :item       {:default {:method      "PUT"
                             :title       (str "replace this " item)
                             :contentType "application/json"
                             :properties  (properties resource)}
                   :patch   {:method      "PATCH"
                             :title       (str "amend this " item)
                             :contentType "application/json"
                             :properties  (properties resource)}
                   :delete  {:method     "DELETE"
                             :title      (str "remove this " item)
                             :properties []}}
      nil)))
