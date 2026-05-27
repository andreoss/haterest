(ns hypermedia.api
  (:require [clojure.string :as str]
            [hypermedia.alps :as alps]
            [hypermedia.etag :as etag]
            [hypermedia.forms :as forms]
            [hypermedia.hal :as hal]
            [hypermedia.health :as health]
            [hypermedia.negotiate :as negotiate]
            [hypermedia.page :as page]
            [hypermedia.problem :as problem]
            [hypermedia.rel :as rel]
            [hypermedia.route :as route]
            [hypermedia.schema :as schema]
            [hypermedia.sql :as sql]
            [hypermedia.store :as store]
            [hypermedia.uri :as uri]
            [hypermedia.urilist :as urilist]
            [jsonista.core :as json]
            [reitit.ring :as ring]
            [ring.middleware.params :as params]))

(def ^:private mapper (json/object-mapper {:encode-key-fn name}))

(def ^:private readable-types #{"application/json" "application/hal+json"})

(def ^:private hal-offers [hal/media-type forms/media-type "application/json"])

(def ^:private alps-offers [alps/media-type "application/json"])

(def ^:private verbs [:get :post :put :patch :delete])

(def ^:private no-content {:status 204 :headers {} :body nil})

(defn- document
  ([resource kind doc] (document resource kind doc 200 nil))
  ([resource kind doc status headers]
   {:status              status
    :headers             (or headers {})
    :hypermedia/document doc
    :hypermedia/resource resource
    :hypermedia/kind     kind}))

(defn- render [media response]
  (if-let [doc (:hypermedia/document response)]
    (let [forms?   (= media forms/media-type)
          resource (:hypermedia/resource response)
          extended (if-let [templates (and forms? resource
                                           (forms/templates resource (:hypermedia/kind response)))]
                     (assoc doc :_templates templates)
                     doc)]
      {:status  (:status response)
       :headers (assoc (:headers response) "Content-Type"
                       (str (if forms? forms/media-type hal/media-type) ";charset=utf-8"))
       :body    (json/write-value-as-string extended mapper)})
    response))

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
  (uri/render (:self-parts resource) {(:identity resource) (get row (:identity resource))}))

(defn- profile-href [resource]
  (str "/profile/" (name (:collection resource))))

(defn- item-doc
  ([model resource row] (item-doc model resource row nil nil))
  ([model resource row embeds] (item-doc model resource row embeds nil))
  ([model resource row embeds projection]
   (let [curie   (:curie model)
         hidden  (conj (foreign-keys resource) (:identity resource))
         binding {(:identity resource) (get row (:identity resource))}
         shown   (if projection (:fields projection) (:field-order resource))
         props   (reduce (fn [m k] (if (or (hidden k) (not (contains? row k)))
                                     m
                                     (assoc m k (get row k))))
                         {} shown)
         self    (uri/render (:self-parts resource) binding)
         links   (cond-> (into {:self    (hal/href self)
                                :profile (hal/href (:profile-path resource))}
                               (for [[k relation] (:relations resource)]
                                 [(rel/curied curie k) (hal/href (uri/render (:parts relation) binding))]))
                   (seq (:projections resource))
                   (assoc (rel/curied curie :projection)
                          (assoc (hal/href (str self "{?projection}")) :templated true)))
         nested  (into {}
                       (for [[k {:keys [kind target index]}] embeds
                             :let [relation (get-in resource [:relations k])
                                   value    (if (= :belongs-to kind)
                                              (get index (get row (:via relation)))
                                              (get index (get row (:identity resource)) []))]
                             :when (some? value)]
                         [k value]))]
     (hal/document props links nested))))

(defn- embedded-relations [resource projection]
  (if projection
    (keep #(get-in resource [:relations %]) (:embed projection))
    (keep (fn [[_ relation]] (when (:embed? relation) relation)) (:relations resource))))

(defn- embeds-for [model store resource rows projection]
  (into {}
        (for [relation (embedded-relations resource projection)
              :let [k      (:name relation)
                    target (get-in model [:resources (:target relation)])
                    render (fn [target-row] (item-doc model target target-row))]]
          [k (case (:kind relation)
               :belongs-to
               (let [ids (into #{} (keep #(get % (:via relation))) rows)]
                 {:kind   :belongs-to
                  :target target
                  :index  (if (seq ids)
                            (into {} (map (juxt (:identity target) render))
                                  (store/query store target {:where {(:identity target) ids}}))
                            {})})

               :many-to-many
               (let [owners (into #{} (keep #(get % (:identity resource))) rows)
                     pairs  (if (seq owners)
                              (store/links-of store resource target relation owners)
                              [])
                     found  (if (seq pairs)
                              (into {} (map (juxt (:identity target) render))
                                    (store/query store target
                                                 {:where {(:identity target) (into #{} (map second) pairs)}}))
                              {})]
                 {:kind   :has-many
                  :target target
                  :index  (reduce (fn [m [owner-id target-id]]
                                    (if-let [found-row (get found target-id)]
                                      (update m owner-id (fnil conj []) found-row)
                                      m))
                                  {} pairs)})

               (let [ids (into #{} (keep #(get % (:identity resource))) rows)]
                 {:kind   :has-many
                  :target target
                  :index  (if (seq ids)
                            (update-vals (group-by #(get % (:via relation))
                                                   (store/query store target
                                                                {:where {(:via relation) ids}}))
                                         #(mapv render %))
                            {})}))])))

(defn- searches-link [model resource]
  (when (seq (:searches resource))
    {(rel/curied (:curie model) :search) (hal/link (:search-path resource))}))

(defn- collection-doc [model store resource rows base pageable total projection]
  (let [embeds (embeds-for model store resource rows projection)]
    (hal/document {:page (page/descriptor pageable total)}
                  (merge (page/links base pageable total)
                         {:profile (hal/link (:profile-path resource))}
                         (searches-link model resource))
                  {(:collection resource) (mapv #(item-doc model resource % embeds projection) rows)})))

(defn- single-doc [model store resource row projection]
  (item-doc model resource row (embeds-for model store resource [row] projection) projection))

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
                                   (hal/link (:profile-path resource))]))
                    (rel/curies curie) (assoc :curies (rel/curies curie))))))

(defn- searches-doc [model resource]
  (let [curie (:curie model)]
    (hal/document {}
                  (into {:self (hal/link (:search-path resource))}
                        (for [[k search] (:searches resource)]
                          [(rel/curied curie k) (hal/link (:template search))])))))

(defn- identity-of [resource request]
  (some->> (get-in request [:path-params (:identity resource)])
           (schema/coerce (get-in resource [:fields (:identity resource) :type]))))

(defn- projection-of [resource request]
  (if-let [asked (get-in request [:query-params "projection"])]
    (if-let [found (get-in resource [:projections (keyword asked)])]
      {:projection found}
      {:problem (problem/of 400 "no such projection"
                            {:instance (:uri request)
                             :detail   (str "this resource offers "
                                            (str/join ", " (map name (keys (:projections resource))))) })})
    {}))

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

(defn- resolve-href [target href]
  (let [prefix (str (:path target) "/")]
    (when (str/starts-with? href prefix)
      (schema/coerce (get-in target [:fields (:identity target) :type])
                     (uri/decode (subs href (count prefix)))))))

(defn- referenced [target request]
  (let [content-type (or (get-in request [:headers "content-type"]) "")]
    (if-not (str/starts-with? content-type urilist/media-type)
      {:problem (problem/of 415 "unsupported media type"
                            {:instance (:uri request) :detail (str "send " urilist/media-type)})}
      (let [raw   (:body request)
            hrefs (urilist/parse (cond (nil? raw) nil (string? raw) raw :else (slurp raw)))
            ids   (mapv #(resolve-href target %) hrefs)]
        (if (some nil? ids)
          {:problem (problem/of 422 "a reference does not name a resource of this kind"
                                {:instance (:uri request)
                                 :errors   (vec (for [[href id] (map vector hrefs ids) :when (nil? id)]
                                                  {:field :uri :error :unreadable :detail href}))})}
          {:ids (vec (distinct ids))})))))

(defn- precondition [request current]
  (let [header (get-in request [:headers "if-match"])]
    (cond
      (str/blank? header) {:expected (store/version-of current)}
      (nil? current)      {:unmatchable true}
      (etag/matches? header (store/version-of current)) {:expected (store/version-of current)}
      :else {:unmatchable true})))

(defn- precondition-failed [request]
  (problem/of 412 "the resource has moved on since it was read" {:instance (:uri request)}))

(defn- refused [request]
  (if (str/blank? (get-in request [:headers "if-match"]))
    (problem/of 409 "the resource was written by someone else at the same time"
                {:instance (:uri request)})
    (precondition-failed request)))

(def ^:private slice-params #{"page" "size" "sort"})

(defn- query-suffix [request]
  (let [kept (sort-by key (remove (fn [[k _]] (contains? slice-params k)) (:query-params request)))]
    (if (seq kept)
      (str "&" (str/join "&" (for [[k v] kept
                                   one (if (sequential? v) v [v])]
                               (str k "=" (uri/encode one)))))
      "")))

(defn- sliced-by [model store resource base request rows-of total-of]
  (let [pageable (page/parse resource (:query-params request))
        {:keys [projection problem]} (projection-of resource request)]
    (cond
      problem problem
      (seq (:errors pageable))
      (problem/of 400 "the slice cannot be read"
                  {:instance (:uri request) :errors (:errors pageable)})
      :else
      (let [rows  (rows-of pageable)
            total (total-of)
            doc   (collection-doc model store resource rows base pageable total projection)
            extra (query-suffix request)]
        (document resource :collection
                  (if (str/blank? extra)
                    doc
                    (update doc :_links
                            #(reduce-kv (fn [m rel link]
                                          (assoc m rel (if (str/includes? (:href link) "page=")
                                                         (update link :href str extra)
                                                         link)))
                                        {} %))))))))

(defn- sliced [model store resource base where request]
  (sliced-by model store resource base request
             (fn [pageable] (store/query store resource (page/criteria pageable where)))
             (fn [] (store/total store resource {:where where}))))

(defn- sliced-through [model store owner target relation owner-id base request]
  (sliced-by model store target base request
             (fn [pageable] (store/linked store owner target relation owner-id
                                          (dissoc (page/criteria pageable {}) :where)))
             (fn [] (store/linked-total store owner target relation owner-id))))

(defn- fresh? [request row]
  (etag/matches? (get-in request [:headers "if-none-match"]) (store/version-of row)))

(defn- item-response [model store resource row request]
  (let [{:keys [projection problem]} (projection-of resource request)]
    (cond
      problem            problem
      (fresh? request row) {:status 304
                            :headers {"ETag" (etag/of (store/version-of row))}
                            :body nil}
      :else (document resource :item (single-doc model store resource row projection) 200
                      {"ETag" (etag/of (store/version-of row))}))))

(defn- handle-collection [model store resource request]
  (sliced model store resource (:path resource) {} request))

(defn- handle-item [model store resource request]
  (if-let [row (some->> (identity-of resource request) (store/fetch store resource))]
    (item-response model store resource row request)
    (missing request)))

(defn- handle-search-index [model resource _]
  (document resource :search-index (searches-doc model resource)))

(defn- handle-search [model store resource search request]
  (let [given   (:query-params request)
        clauses (for [field (:predicates search)
                      :let [value (get given (name field))]
                      :when (some? value)]
                  [field (schema/coerce (get-in resource [:fields field :type]) value)])]
    (if (some (comp nil? second) clauses)
      (problem/of 400 "a predicate cannot be read"
                  {:instance (:uri request)
                   :errors   (vec (for [[field value] clauses :when (nil? value)]
                                    {:field field :error :unreadable}))})
      (sliced model store resource (:path search) (into {} clauses) request))))

(defn- handle-association [model store resource relation request]
  (let [target (get-in model [:resources (:target relation)])
        id     (identity-of resource request)
        row    (some->> id (store/fetch store resource))
        self   (uri/render (:parts relation) {(:identity resource) id})]
    (cond
      (nil? row) (missing request)
      (= :many-to-many (:kind relation))
      (sliced-through model store resource target relation id self request)
      (= :has-many (:kind relation)) (sliced model store target self {(:via relation) id} request)
      :else (if-let [linked (some->> (get row (:via relation)) (store/fetch store target))]
              (item-response model store target linked request)
              (missing request)))))

(defn- handle-create [model store resource request]
  (let [{:keys [row problem]} (submitted resource request {})]
    (or problem
        (store/transact store
                        (fn [tx]
                          (let [stored (store/create! tx resource row)]
                            (document resource :item (single-doc model tx resource stored nil) 201
                                      {"Location" (self-href resource stored)
                                       "ETag"     (etag/of (store/version-of stored))})))))))

(defn- handle-replace [model store resource request]
  (let [id (identity-of resource request)
        {:keys [row problem]} (submitted resource request {:identity id})]
    (cond
      (nil? id) (missing request)
      problem   problem
      :else
      (store/transact
       store
       (fn [tx]
         (let [current (store/fetch tx resource id)
               {:keys [expected unmatchable]} (precondition request current)]
           (if unmatchable
             (refused request)
             (let [written (store/replace! tx resource id row expected)]
               (if (contains? #{:stale :conflict} (:outcome written))
                 (refused request)
                 (document resource :item
                           (single-doc model tx resource (:row written) nil)
                           (if (= :created (:outcome written)) 201 200)
                           {"Location" (uri/render (:self-parts resource) {(:identity resource) id})
                            "ETag"     (etag/of (store/version-of (:row written)))}))))))))))

(defn- handle-amend [model store resource request]
  (let [id (identity-of resource request)
        {:keys [row problem]} (submitted resource request {:partial? true})]
    (cond
      (nil? id) (missing request)
      problem   problem
      :else
      (store/transact
       store
       (fn [tx]
         (let [current (store/fetch tx resource id)
               {:keys [expected unmatchable]} (precondition request current)]
           (cond
             (nil? current) (missing request)
             unmatchable    (refused request)
             :else
             (let [written (store/amend! tx resource id row expected)]
               (case (:outcome written)
                 :absent            (missing request)
                 (:stale :conflict) (refused request)
                 (document resource :item
                           (single-doc model tx resource (:row written) nil) 200
                           {"ETag" (etag/of (store/version-of (:row written)))}))))))))))

(defn- handle-erase [store resource request]
  (let [id (identity-of resource request)]
    (if (nil? id)
      (missing request)
      (store/transact
       store
       (fn [tx]
         (let [current (store/fetch tx resource id)
               {:keys [expected unmatchable]} (precondition request current)]
           (cond
             (nil? current) (missing request)
             unmatchable    (refused request)
             :else
             (case (:outcome (store/erase! tx resource id expected))
               :absent            (missing request)
               (:stale :conflict) (refused request)
               no-content))))))))

(defn- claim-owner
  [store resource id request]
  (let [current (store/fetch store resource id)
        {:keys [expected unmatchable]} (precondition request current)]
    (cond
      (nil? current) {:outcome :absent}
      unmatchable    {:outcome :stale}
      :else          (store/amend! store resource id {} expected))))

(defn- all-present? [store target ids]
  (or (empty? ids)
      (= (count ids)
         (count (store/query store target {:where {(:identity target) (set ids)}})))))

(defn- handle-bind [model store resource relation request]
  (let [target (get-in model [:resources (:target relation)])
        id     (identity-of resource request)
        {:keys [ids problem]} (referenced target request)]
    (cond
      problem problem

      (and (= :belongs-to (:kind relation)) (not= 1 (count ids)))
      (problem/of 422 "this relation holds one resource" {:instance (:uri request)})

      (not (all-present? store target ids))
      (problem/of 422 "the referenced resource does not exist" {:instance (:uri request)})

      :else
      (store/transact
       store
       (fn [tx]
         (let [claimed (if (= :belongs-to (:kind relation))
                         (let [current (store/fetch tx resource id)
                               {:keys [expected unmatchable]} (precondition request current)]
                           (if unmatchable
                             {:outcome :stale}
                             (store/amend! tx resource id {(:via relation) (first ids)} expected)))
                         (claim-owner tx resource id request))]
           (case (:outcome claimed)
             :absent            (missing request)
             (:stale :conflict) (refused request)
             (do
               (when (= :many-to-many (:kind relation))
                 (when (= :put (:request-method request))
                   (store/unlink! tx resource target relation id nil))
                 (let [already (into #{} (map second) (store/links-of tx resource target relation [id]))]
                   (store/link! tx resource target relation id (remove already ids))))
               (when (= :has-many (:kind relation))
                 (when (= :put (:request-method request))
                   (store/amend-where! tx target {(:via relation) id} {(:via relation) nil}))
                 (when (seq ids)
                   (store/amend-where! tx target {(:identity target) (set ids)}
                                       {(:via relation) id})))
               no-content))))))))

(defn- handle-unbind [model store resource relation request]
  (let [id (identity-of resource request)]
    (store/transact
     store
     (fn [tx]
       (if-not (= :belongs-to (:kind relation))
         (missing request)
         (let [current (store/fetch tx resource id)
               {:keys [expected unmatchable]} (precondition request current)]
           (cond
             (nil? current) (missing request)
             unmatchable    (refused request)
             :else
             (case (:outcome (store/amend! tx resource id {(:via relation) nil} expected))
               :absent            (missing request)
               (:stale :conflict) (refused request)
               no-content))))))))

(defn- handle-unbind-member [model store resource relation request]
  (let [target   (get-in model [:resources (:target relation)])
        id       (identity-of resource request)
        member   (some->> (get-in request [:path-params (route/member-variable relation)])
                          (schema/coerce (get-in target [:fields (:identity target) :type])))
        owner    (some->> id (store/fetch store resource))
        referent (some->> member (store/fetch store target))]
    (cond
      (not (and owner referent)) (missing request)

      (= :many-to-many (:kind relation))
      (store/transact
       store
       (fn [tx]
         (let [claimed (claim-owner tx resource id request)]
           (case (:outcome claimed)
             :absent            (missing request)
             (:stale :conflict) (refused request)
             (if (pos? (store/unlink! tx resource target relation id [member]))
               no-content
               (missing request))))))

      (= id (get referent (:via relation)))
      (store/transact
       store
       (fn [tx]
         (let [claimed (claim-owner tx resource id request)]
           (case (:outcome claimed)
             :absent            (missing request)
             (:stale :conflict) (refused request)
             (do (store/amend-where! tx target {(:identity target) member} {(:via relation) nil})
                 no-content)))))

      :else (missing request))))

(defn- endpoints [model store data]
  (let [resource (get-in model [:resources (:hypermedia/resource data)])
        relation (:hypermedia/relation data)
        search   (:hypermedia/search data)]
    (case (:hypermedia/op data)
      :root         {:get (fn [_] (document nil :root (root-doc model)))}
      :collection   {:get  (partial handle-collection model store resource)
                     :post (partial handle-create model store resource)}
      :item         {:get    (partial handle-item model store resource)
                     :put    (partial handle-replace model store resource)
                     :patch  (partial handle-amend model store resource)
                     :delete (partial handle-erase store resource)}
      :search-index {:get (partial handle-search-index model resource)}
      :search       {:get (partial handle-search model store resource search)}
      :association  (cond-> {:get (partial handle-association model store resource relation)
                             :put (partial handle-bind model store resource relation)}
                      (= :belongs-to (:kind relation))
                      (assoc :delete (partial handle-unbind model store resource relation))
                      (contains? #{:has-many :many-to-many} (:kind relation))
                      (assoc :post (partial handle-bind model store resource relation)))
      :association-member {:delete (partial handle-unbind-member model store resource relation)})))

(defn- negotiating [offers handlers]
  (reduce-kv (fn [m method handler]
               (assoc m method
                      (fn [request]
                        (if-let [media (negotiate/choose (get-in request [:headers "accept"]) offers)]
                          (render media (handler request))
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
  (let [handlers (endpoints model store data)]
    (assoc (negotiating hal-offers handlers) :options (options-endpoint handlers))))

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
                      (if-let [media (negotiate/choose (get-in request [:headers "accept"]) hal-offers)]
                        (render media (document nil :profile (profile-doc model)))
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
  {:model model :routes (route/routes model) :statements (sql/statements model)})

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

(defn- report [request exception]
  (binding [*out* *err*]
    (println (str "unserved " (str/upper-case (name (:request-method request))) " " (:uri request)
                  ": " (.getName (class exception)) " " (.getMessage ^Exception exception)))
    (doseq [frame (take 6 (.getStackTrace ^Exception exception))]
      (println "   " (str frame)))))

(defn- guarded [handler]
  (fn [request]
    (try (handler request)
         (catch Exception e
           (report request e)
           (problem/of 500 "the request could not be served" {:instance (:uri request)})))))

(defn handler [api store]
  (let [model  (:model api)
        routes (-> (mapv (fn [[path data]] [path (merge data (endpoint-map model store data))])
                         (:routes api))
                   (into (profile-routes model))
                   (conj ["/health" {:name :hypermedia.route/health :get (health-endpoint store)}]))]
    (ring/ring-handler (ring/router routes {:conflicts nil})
                       (default-handler)
                       {:middleware [guarded params/wrap-params keywordise-params]})))

(defmacro defapi [sym config]
  (let [model (schema/parse (eval config))]
    `(def ~sym {:model      ~model
                :routes     ~(route/routes model)
                :statements ~(sql/statements model)})))
