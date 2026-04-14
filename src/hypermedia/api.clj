(ns hypermedia.api
  (:require [hypermedia.hal :as hal]
            [hypermedia.health :as health]
            [hypermedia.route :as route]
            [hypermedia.schema :as schema]
            [hypermedia.store :as store]
            [hypermedia.uri :as uri]
            [jsonista.core :as json]
            [reitit.ring :as ring]))

(def ^:private mapper (json/object-mapper {:encode-key-fn name}))

(defn- respond
  ([body] (respond 200 body))
  ([status body]
   {:status  status
    :headers {"Content-Type" (str hal/media-type ";charset=utf-8")}
    :body    (json/write-value-as-string body mapper)}))

(defn- not-found []
  (respond 404 (hal/document {:status 404 :title "not found"} {})))

(defn- foreign-keys [resource]
  (into #{} (keep (fn [[_ relation]] (when (= :belongs-to (:kind relation)) (:via relation))))
        (:relations resource)))

(defn- self-template [resource]
  (str (:path resource) "/{" (name (:identity resource)) "}"))

(defn- item-doc [resource row]
  (let [id      (get row (:identity resource))
        hidden  (conj (foreign-keys resource) (:identity resource))
        binding {(:identity resource) id}
        props   (reduce (fn [m k] (if (or (hidden k) (not (contains? row k)))
                                    m
                                    (assoc m k (get row k))))
                        {} (:field-order resource))
        links   (into {:self (hal/link (uri/expand (self-template resource) binding))}
                      (for [[k relation] (:relations resource)]
                        [k (hal/link (uri/expand (:path relation) binding))]))]
    (hal/document props links)))

(defn- collection-doc [resource rows self]
  (hal/document {}
                {:self (hal/link self)}
                {(:collection resource) (mapv #(item-doc resource %) rows)}))

(defn- root-doc [model]
  (hal/document {}
                (into {:self (hal/link "/")
                       :health (hal/link "/health")}
                      (for [k (:order model)
                            :let [resource (get-in model [:resources k])]]
                        [(:collection resource) (hal/link (:path resource))]))))

(defn- identity-of [resource request]
  (some->> (get-in request [:path-params (:identity resource)])
           (schema/coerce (get-in resource [:fields (:identity resource) :type]))))

(defn- handle-item [model store resource request]
  (if-let [row (some->> (identity-of resource request) (store/fetch store resource))]
    (respond (item-doc resource row))
    (not-found)))

(defn- handle-collection [model store resource request]
  (respond (collection-doc resource (store/query store resource {}) (:path resource))))

(defn- handle-association [model store resource relation request]
  (let [target (get-in model [:resources (:target relation)])
        id     (identity-of resource request)
        row    (some->> id (store/fetch store resource))
        self   (uri/expand (:path relation) {(:identity resource) id})]
    (cond
      (nil? row)                        (not-found)
      (= :has-many (:kind relation))    (respond (collection-doc target
                                                                 (store/query store target {:where {(:via relation) id}})
                                                                 self))
      :else (if-let [linked (some->> (get row (:via relation)) (store/fetch store target))]
              (respond (item-doc target linked))
              (not-found)))))

(defn- endpoint [model store data]
  (let [resource (get-in model [:resources (:hypermedia/resource data)])
        relation (:hypermedia/relation data)]
    (case (:hypermedia/op data)
      :root        (fn [_] (respond (root-doc model)))
      :collection  (partial handle-collection model store resource)
      :item        (partial handle-item model store resource)
      :association (partial handle-association model store resource relation))))

(defn- keywordise-params [handler]
  (fn [request]
    (handler (update request :path-params #(into {} (map (fn [[k v]] [(keyword k) v])) %)))))

(defn- health-endpoint [store]
  (fn [_]
    (let [report (health/report store)]
      {:status  (if (health/up? report) 200 503)
       :headers {"Content-Type" "application/json;charset=utf-8"}
       :body    (json/write-value-as-string report mapper)})))

(defn build [model]
  {:model model :routes (route/routes model)})

(defn handler [api store]
  (let [model  (:model api)
        routes (conj (mapv (fn [[path data]] [path (assoc data :get (endpoint model store data))])
                           (:routes api))
                     ["/health" {:name :hypermedia.route/health :get (health-endpoint store)}])]
    (ring/ring-handler (ring/router routes)
                       (fn [_] (not-found))
                       {:middleware [keywordise-params]})))

(defmacro defapi [sym config]
  (let [model (schema/parse (eval config))]
    `(def ~sym {:model ~model :routes ~(route/routes model)})))
