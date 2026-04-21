(ns hypermedia.api
  (:require [clojure.string :as str]
            [hypermedia.hal :as hal]
            [hypermedia.health :as health]
            [hypermedia.problem :as problem]
            [hypermedia.route :as route]
            [hypermedia.schema :as schema]
            [hypermedia.store :as store]
            [hypermedia.uri :as uri]
            [jsonista.core :as json]
            [reitit.ring :as ring]))

(def ^:private mapper (json/object-mapper {:encode-key-fn name}))

(def ^:private readable-types #{"application/json" "application/hal+json"})

(defn- respond
  ([body] (respond 200 body nil))
  ([status body] (respond status body nil))
  ([status body headers]
   {:status  status
    :headers (merge {"Content-Type" (str hal/media-type ";charset=utf-8")} headers)
    :body    (when body (json/write-value-as-string body mapper))}))

(defn- foreign-keys [resource]
  (into #{} (keep (fn [[_ relation]] (when (= :belongs-to (:kind relation)) (:via relation))))
        (:relations resource)))

(defn- self-template [resource]
  (str (:path resource) "/{" (name (:identity resource)) "}"))

(defn- self-href [resource row]
  (uri/expand (self-template resource) {(:identity resource) (get row (:identity resource))}))

(defn- item-doc [resource row]
  (let [hidden  (conj (foreign-keys resource) (:identity resource))
        binding {(:identity resource) (get row (:identity resource))}
        props   (reduce (fn [m k] (if (or (hidden k) (not (contains? row k)))
                                    m
                                    (assoc m k (get row k))))
                        {} (:field-order resource))
        links   (into {:self (hal/link (self-href resource row))}
                      (for [[k relation] (:relations resource)]
                        [k (hal/link (uri/expand (:path relation) binding))]))]
    (hal/document props links)))

(defn- collection-doc [resource rows self]
  (hal/document {}
                {:self (hal/link self)}
                {(:collection resource) (mapv #(item-doc resource %) rows)}))

(defn- root-doc [model]
  (hal/document {}
                (into {:self   (hal/link "/")
                       :health (hal/link "/health")}
                      (for [k (:order model)
                            :let [resource (get-in model [:resources k])]]
                        [(:collection resource) (hal/link (:path resource))]))))

(defn- identity-of [resource request]
  (some->> (get-in request [:path-params (:identity resource)])
           (schema/coerce (get-in resource [:fields (:identity resource) :type]))))

(defn- missing [request]
  (problem/of 404 "not found" {:instance (:uri request)}))

(defn- body-of [request]
  (let [content-type (or (get-in request [:headers "content-type"]) "")]
    (if (some #(str/starts-with? content-type %) readable-types)
      (try {:value (json/read-value (:body request) json/keyword-keys-object-mapper)}
           (catch Exception _ {:problem (problem/of 400 "unreadable body" {:instance (:uri request)})}))
      {:problem (problem/of 415 "unsupported media type"
                            {:instance (:uri request) :detail "send application/json"})})))

(defn- submitted [resource request options]
  (let [{:keys [value problem]} (body-of request)]
    (cond
      problem       {:problem problem}
      (not (map? value)) {:problem (problem/of 400 "unreadable body" {:instance (:uri request)})}
      :else (let [{:keys [value errors]} (schema/conform resource value options)]
              (cond
                (some #(= :conflict (:error %)) errors)
                {:problem (problem/of 409 "the submitted identity is not the one addressed"
                                      {:instance (:uri request) :errors errors})}
                (seq errors)
                {:problem (problem/of 422 "the submission does not fit the schema"
                                      {:instance (:uri request) :errors errors})}
                :else {:row value})))))

(defn- handle-item [store resource request]
  (if-let [row (some->> (identity-of resource request) (store/fetch store resource))]
    (respond (item-doc resource row))
    (missing request)))

(defn- handle-collection [store resource _]
  (respond (collection-doc resource (store/query store resource {}) (:path resource))))

(defn- handle-association [model store resource relation request]
  (let [target (get-in model [:resources (:target relation)])
        id     (identity-of resource request)
        row    (some->> id (store/fetch store resource))
        self   (uri/expand (:path relation) {(:identity resource) id})]
    (cond
      (nil? row) (missing request)
      (= :has-many (:kind relation))
      (respond (collection-doc target (store/query store target {:where {(:via relation) id}}) self))
      :else (if-let [linked (some->> (get row (:via relation)) (store/fetch store target))]
              (respond (item-doc target linked))
              (missing request)))))

(defn- handle-create [store resource request]
  (let [{:keys [row problem]} (submitted resource request {})]
    (or problem
        (store/transact store
                        (fn [tx]
                          (let [stored (store/create! tx resource row)]
                            (respond 201 (item-doc resource stored)
                                     {"Location" (self-href resource stored)})))))))

(defn- handle-replace [store resource request]
  (let [id (identity-of resource request)
        {:keys [row problem]} (submitted resource request {:identity id})]
    (cond
      (nil? id) (missing request)
      problem   problem
      :else (store/transact store
                            (fn [tx]
                              (let [{:keys [created? row]} (store/replace! tx resource id row)]
                                (respond (if created? 201 200) (item-doc resource row)
                                         {"Location" (self-href resource row)})))))))

(defn- handle-amend [store resource request]
  (let [id (identity-of resource request)
        {:keys [row problem]} (submitted resource request {:partial? true})]
    (cond
      (nil? id) (missing request)
      problem   problem
      :else (store/transact store
                            (fn [tx]
                              (if-let [stored (store/amend! tx resource id row)]
                                (respond (item-doc resource stored))
                                (missing request)))))))

(defn- handle-erase [store resource request]
  (let [id (identity-of resource request)]
    (if (and id (store/transact store (fn [tx] (store/erase! tx resource id))))
      {:status 204 :headers {} :body nil}
      (missing request))))

(defn- endpoints [model store data]
  (let [resource (get-in model [:resources (:hypermedia/resource data)])
        relation (:hypermedia/relation data)]
    (case (:hypermedia/op data)
      :root        {:get (fn [_] (respond (root-doc model)))}
      :collection  {:get  (partial handle-collection store resource)
                    :post (partial handle-create store resource)}
      :item        {:get    (partial handle-item store resource)
                    :put    (partial handle-replace store resource)
                    :patch  (partial handle-amend store resource)
                    :delete (partial handle-erase store resource)}
      :association {:get (partial handle-association model store resource relation)})))

(defn- health-endpoint [store]
  (fn [_]
    (let [report (health/report store)]
      {:status  (if (health/up? report) 200 503)
       :headers {"Content-Type" "application/json;charset=utf-8"}
       :body    (json/write-value-as-string report mapper)})))

(defn build [model]
  {:model model :routes (route/routes model)})

(defn- keywordise-params [handler]
  (fn [request]
    (handler (update request :path-params #(into {} (map (fn [[k v]] [(keyword k) v])) %)))))

(defn- default-handler []
  (ring/routes
   (ring/create-default-handler
    {:not-found          (fn [request] (missing request))
     :method-not-allowed (fn [request] (problem/of 405 "method not allowed" {:instance (:uri request)}))
     :not-acceptable     (fn [request] (problem/of 406 "not acceptable" {:instance (:uri request)}))})))

(defn handler [api store]
  (let [model  (:model api)
        routes (conj (mapv (fn [[path data]] [path (merge data (endpoints model store data))])
                           (:routes api))
                     ["/health" {:name :hypermedia.route/health :get (health-endpoint store)}])]
    (ring/ring-handler (ring/router routes)
                       (default-handler)
                       {:middleware [keywordise-params]})))

(defmacro defapi [sym config]
  (let [model (schema/parse (eval config))]
    `(def ~sym {:model ~model :routes ~(route/routes model)})))
