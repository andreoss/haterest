(ns hypermedia.api
  (:require [clojure.string :as str]
            [hypermedia.alps :as alps]
            [hypermedia.hal :as hal]
            [hypermedia.health :as health]
            [hypermedia.negotiate :as negotiate]
            [hypermedia.page :as page]
            [hypermedia.problem :as problem]
            [hypermedia.rel :as rel]
            [hypermedia.route :as route]
            [hypermedia.schema :as schema]
            [hypermedia.store :as store]
            [hypermedia.uri :as uri]
            [jsonista.core :as json]
            [reitit.ring :as ring]
            [ring.middleware.params :as params]))

(def ^:private mapper (json/object-mapper {:encode-key-fn name}))

(def ^:private readable-types #{"application/json" "application/hal+json"})

(def ^:private hal-offers ["application/hal+json" "application/json"])

(def ^:private alps-offers ["application/alps+json" "application/json"])

(def ^:private verbs [:get :post :put :patch :delete])

(defn- respond
  ([body] (respond 200 body nil))
  ([status body] (respond status body nil))
  ([status body headers]
   {:status  status
    :headers (merge {"Content-Type" (str hal/media-type ";charset=utf-8")} headers)
    :body    (when body (json/write-value-as-string body mapper))}))

(defn- as-type [media body]
  {:status  200
   :headers {"Content-Type" (str media ";charset=utf-8")}
   :body    (json/write-value-as-string body mapper)})

(defn- missing [request]
  (problem/of 404 "not found" {:instance (:uri request)}))

(defn- foreign-keys [resource]
  (into #{} (keep (fn [[_ relation]] (when (= :belongs-to (:kind relation)) (:via relation))))
        (:relations resource)))

(defn- self-template [resource]
  (str (:path resource) "/{" (name (:identity resource)) "}"))

(defn- self-href [resource row]
  (uri/expand (self-template resource) {(:identity resource) (get row (:identity resource))}))

(defn- profile-href [resource]
  (str "/profile/" (name (:collection resource))))

(defn- item-doc [model resource row]
  (let [curie   (:curie model)
        hidden  (conj (foreign-keys resource) (:identity resource))
        binding {(:identity resource) (get row (:identity resource))}
        props   (reduce (fn [m k] (if (or (hidden k) (not (contains? row k)))
                                    m
                                    (assoc m k (get row k))))
                        {} (:field-order resource))
        links   (into {:self    (hal/link (self-href resource row))
                       :profile (hal/link (profile-href resource))}
                      (for [[k relation] (:relations resource)]
                        [(rel/curied curie k) (hal/link (uri/expand (:path relation) binding))]))]
    (hal/document props links)))

(defn- collection-doc [model resource rows base pageable total]
  (hal/document {:page (page/descriptor pageable total)}
                (assoc (page/links base pageable total)
                       :profile (hal/link (profile-href resource)))
                {(:collection resource) (mapv #(item-doc model resource %) rows)}))

(defn- root-doc [model]
  (let [curie (:curie model)]
    (hal/document {}
                  (cond-> (into {:self    (hal/link "/")
                                 :profile (hal/link "/profile")
                                 (rel/curied curie :health) (hal/link "/health")}
                                (for [k (:order model)
                                      :let [resource (get-in model [:resources k])]]
                                  [(rel/curied curie (:collection resource)) (hal/link (:path resource))]))
                    (rel/curies curie) (assoc :curies (rel/curies curie))))))

(defn- profile-doc [model]
  (let [curie (:curie model)]
    (hal/document {}
                  (cond-> (into {:self (hal/link "/profile")}
                                (for [k (:order model)
                                      :let [resource (get-in model [:resources k])]]
                                  [(rel/curied curie (:collection resource))
                                   (hal/link (profile-href resource))]))
                    (rel/curies curie) (assoc :curies (rel/curies curie))))))

(defn- identity-of [resource request]
  (some->> (get-in request [:path-params (:identity resource)])
           (schema/coerce (get-in resource [:fields (:identity resource) :type]))))

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
      problem            {:problem problem}
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

(defn- sliced [model store resource base where request]
  (let [pageable (page/parse resource (:query-params request))]
    (if (seq (:errors pageable))
      (problem/of 400 "the slice cannot be read"
                  {:instance (:uri request) :errors (:errors pageable)})
      (respond (collection-doc model resource
                               (store/query store resource (page/criteria pageable where))
                               base pageable
                               (store/total store resource {:where where}))))))

(defn- handle-collection [model store resource request]
  (sliced model store resource (:path resource) {} request))

(defn- handle-item [model store resource request]
  (if-let [row (some->> (identity-of resource request) (store/fetch store resource))]
    (respond (item-doc model resource row))
    (missing request)))

(defn- handle-association [model store resource relation request]
  (let [target (get-in model [:resources (:target relation)])
        id     (identity-of resource request)
        row    (some->> id (store/fetch store resource))
        self   (uri/expand (:path relation) {(:identity resource) id})]
    (cond
      (nil? row) (missing request)
      (= :has-many (:kind relation)) (sliced model store target self {(:via relation) id} request)
      :else (if-let [linked (some->> (get row (:via relation)) (store/fetch store target))]
              (respond (item-doc model target linked))
              (missing request)))))

(defn- handle-create [model store resource request]
  (let [{:keys [row problem]} (submitted resource request {})]
    (or problem
        (store/transact store
                        (fn [tx]
                          (let [stored (store/create! tx resource row)]
                            (respond 201 (item-doc model resource stored)
                                     {"Location" (self-href resource stored)})))))))

(defn- handle-replace [model store resource request]
  (let [id (identity-of resource request)
        {:keys [row problem]} (submitted resource request {:identity id})]
    (cond
      (nil? id) (missing request)
      problem   problem
      :else (store/transact store
                            (fn [tx]
                              (let [{:keys [created? row]} (store/replace! tx resource id row)]
                                (respond (if created? 201 200) (item-doc model resource row)
                                         {"Location" (self-href resource row)})))))))

(defn- handle-amend [model store resource request]
  (let [id (identity-of resource request)
        {:keys [row problem]} (submitted resource request {:partial? true})]
    (cond
      (nil? id) (missing request)
      problem   problem
      :else (store/transact store
                            (fn [tx]
                              (if-let [stored (store/amend! tx resource id row)]
                                (respond (item-doc model resource stored))
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
      :collection  {:get  (partial handle-collection model store resource)
                    :post (partial handle-create model store resource)}
      :item        {:get    (partial handle-item model store resource)
                    :put    (partial handle-replace model store resource)
                    :patch  (partial handle-amend model store resource)
                    :delete (partial handle-erase store resource)}
      :association {:get (partial handle-association model store resource relation)})))


(defn- negotiating [offers handlers]
  (reduce-kv (fn [m method handler]
               (assoc m method
                      (fn [request]
                        (if (negotiate/choose (get-in request [:headers "accept"]) offers)
                          (handler request)
                          (problem/of 406 "not acceptable"
                                      {:instance (:uri request)
                                       :detail   (str "this resource offers " (str/join ", " offers))})))))
             {} handlers))

(defn- allow-header [available]
  (str/join ", " (concat (map (comp str/upper-case name) (filter #(contains? available %) verbs))
                         ["OPTIONS"])))

(defn- options-endpoint [handlers]
  (let [allow (allow-header handlers)]
    (fn [_] {:status 200 :headers {"Allow" allow} :body nil})))

(defn- endpoint-map [model store data]
  (let [handlers (negotiating hal-offers (endpoints model store data))]
    (assoc handlers :options (options-endpoint handlers))))

(defn- health-endpoint [store]
  (fn [_]
    (let [report (health/report store)]
      {:status  (if (health/up? report) 200 503)
       :headers {"Content-Type" "application/json;charset=utf-8"}
       :body    (json/write-value-as-string report mapper)})))

(defn- profile-routes [model]
  (into [["/profile"
          {:name    :hypermedia.route/profile
           :get     (fn [request]
                      (if (negotiate/choose (get-in request [:headers "accept"]) hal-offers)
                        (respond (profile-doc model))
                        (problem/of 406 "not acceptable" {:instance (:uri request)})))
           :options (fn [_] {:status 200 :headers {"Allow" "GET, OPTIONS"} :body nil})}]]
        (for [k (:order model)
              :let [resource (get-in model [:resources k])]]
          [(profile-href resource)
           {:name    (keyword "hypermedia.route" (str (name k) ".profile"))
            :get     (fn [request]
                       (if-let [media (negotiate/choose (get-in request [:headers "accept"]) alps-offers)]
                         (as-type media (alps/descriptor model resource))
                         (problem/of 406 "not acceptable" {:instance (:uri request)})))
            :options (fn [_] {:status 200 :headers {"Allow" "GET, OPTIONS"} :body nil})}])))

(defn build [model]
  {:model model :routes (route/routes model)})

(defn- keywordise-params [handler]
  (fn [request]
    (handler (update request :path-params #(into {} (map (fn [[k v]] [(keyword k) v])) %)))))

(defn- default-handler []
  (ring/create-default-handler
   {:not-found          (fn [request] (missing request))
    :method-not-allowed (fn [request]
                          (update (problem/of 405 "method not allowed" {:instance (:uri request)})
                                  :headers assoc "Allow"
                                  (allow-header (get-in request [:reitit.core/match :data]))))
    :not-acceptable     (fn [request] (problem/of 406 "not acceptable" {:instance (:uri request)}))}))

(defn handler [api store]
  (let [model  (:model api)
        routes (-> (mapv (fn [[path data]] [path (merge data (endpoint-map model store data))])
                         (:routes api))
                   (into (profile-routes model))
                   (conj ["/health" {:name :hypermedia.route/health :get (health-endpoint store)}]))]
    (ring/ring-handler (ring/router routes)
                       (default-handler)
                       {:middleware [params/wrap-params keywordise-params]})))

(defmacro defapi [sym config]
  (let [model (schema/parse (eval config))]
    `(def ~sym {:model ~model :routes ~(route/routes model)})))
