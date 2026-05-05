(ns hypermedia.schema-test
  (:require [clojure.test :refer [deftest is testing]]
            [hypermedia.schema :as schema]))

(def config
  {:resources
   {:author {:fields    {:id   {:type :uuid :identity true}
                         :name {:type :string :required true}}
             :relations {:books {:kind :has-many :target :book :via :author-id}}}
    :book   {:collection :books
             :fields     {:id        {:type :uuid :identity true}
                          :title     {:type :string :required true}
                          :author-id {:type :uuid}}
             :relations  {:author {:kind :belongs-to :target :author :via :author-id}}}}})

(deftest parses-resources
  (let [model (schema/parse config)]
    (is (= #{:author :book} (set (keys (:resources model)))))
    (is (= [:author :book] (:order model)))))

(deftest derives-defaults
  (let [author (get-in (schema/parse config) [:resources :author])]
    (is (= :author (:name author)))
    (is (= :authors (:collection author)))
    (is (= "/authors" (:path author)))
    (is (= :authors (:table author)))
    (is (= :id (:identity author)))))

(deftest honours-explicit-collection
  (let [book (get-in (schema/parse config) [:resources :book])]
    (is (= :books (:collection book)))
    (is (= "/books" (:path book)))))

(deftest normalises-fields
  (let [fields (get-in (schema/parse config) [:resources :book :fields])]
    (is (= [:id :title :author-id] (get-in (schema/parse config) [:resources :book :field-order])))
    (is (true? (get-in fields [:id :identity?])))
    (is (true? (get-in fields [:title :required?])))
    (is (false? (get-in fields [:author-id :required?])))
    (is (= :author_id (get-in fields [:author-id :column])))))

(deftest normalises-relations
  (let [rel (get-in (schema/parse config) [:resources :book :relations :author])]
    (is (= :author (:name rel)))
    (is (= :belongs-to (:kind rel)))
    (is (= :author (:target rel)))
    (is (= :author-id (:via rel)))
    (is (= "/books/{id}/author" (:path rel)))))

(deftest rejects-malformed-config
  (testing "a resource without identity"
    (is (thrown? clojure.lang.ExceptionInfo
                 (schema/parse {:resources {:a {:fields {:x {:type :string}}}}}))))
  (testing "a relation to an unknown resource"
    (is (thrown? clojure.lang.ExceptionInfo
                 (schema/parse {:resources {:a {:fields    {:id {:type :uuid :identity true}}
                                                :relations {:b {:kind :belongs-to :target :nope :via :id}}}}}))))
  (testing "an unknown field type"
    (is (thrown? clojure.lang.ExceptionInfo
                 (schema/parse {:resources {:a {:fields {:id {:type :wat :identity true}}}}}))))
  (testing "two resources claiming one path"
    (is (thrown? clojure.lang.ExceptionInfo
                 (schema/parse {:resources {:a {:collection :xs :fields {:id {:type :uuid :identity true}}}
                                            :b {:collection :xs :fields {:id {:type :uuid :identity true}}}}})))))

(deftest reports-every-error-at-once
  (let [e (try (schema/parse {:resources {:a {:fields {:x {:type :wat}}}}})
               (catch clojure.lang.ExceptionInfo e e))]
    (is (<= 2 (count (:errors (ex-data e)))))))

(deftest coerces-path-values-by-type
  (is (= 7 (schema/coerce :long "7")))
  (is (= "7" (schema/coerce :string "7")))
  (is (= (java.util.UUID/fromString "0-0-0-0-1")
         (schema/coerce :uuid "0-0-0-0-1")))
  (is (true? (schema/coerce :boolean "true")))
  (is (nil? (schema/coerce :long "seven"))))

(def writable
  (schema/parse
   {:resources
    {:author {:fields {:id {:type :uuid :identity true :generated true}
                       :name {:type :string :required true}}}
     :book   {:fields    {:id        {:type :uuid :identity true :generated true}
                          :title     {:type :string :required true}
                          :year      {:type :long}
                          :author-id {:type :uuid}}
              :relations {:author {:kind :belongs-to :target :author :via :author-id}}}}}))

(def book (get-in writable [:resources :book]))

(deftest marks-a-generated-identity
  (is (true? (get-in book [:fields :id :generated?]))))

(deftest rejects-generation-of-what-it-cannot-generate
  (is (thrown? clojure.lang.ExceptionInfo
               (schema/parse {:resources {:a {:fields {:id {:type :long :identity true :generated true}}}}})))
  (is (thrown? clojure.lang.ExceptionInfo
               (schema/parse {:resources {:a {:fields {:id {:type :uuid :identity true}
                                                       :n  {:type :uuid :generated true}}}}}))))

(deftest conforms-a-submitted-row
  (let [{:keys [value errors]} (schema/conform book {:title "Dune" :year 1965} {})]
    (is (empty? errors))
    (is (= "Dune" (:title value)))
    (is (= 1965 (:year value)))
    (is (uuid? (:id value)))))

(deftest conforms-numbers-arriving-as-text
  (is (= 1965 (:year (:value (schema/conform book {:title "Dune" :year "1965"} {}))))))

(deftest keeps-a-supplied-identity
  (let [id (random-uuid)]
    (is (= id (:id (:value (schema/conform book {:id (str id) :title "Dune"} {})))))))

(deftest reports-a-missing-required-field
  (is (= [:title] (mapv :field (:errors (schema/conform book {:year 1965} {}))))))

(deftest a-partial-row-needs-nothing
  (let [{:keys [value errors]} (schema/conform book {:year 1965} {:partial? true})]
    (is (empty? errors))
    (is (= {:year 1965} value))))

(deftest reports-an-unknown-field
  (is (= [:sneaky] (mapv :field (:errors (schema/conform book {:title "x" :sneaky 1} {}))))))

(deftest reports-a-value-it-cannot-read
  (is (= [:year] (mapv :field (:errors (schema/conform book {:title "x" :year "soon"} {}))))))

(deftest reads-a-relation-given-as-a-link
  (let [id (random-uuid)]
    (is (= id (:author-id (:value (schema/conform book {:title "x" :author (str "/authors/" id)} {})))))
    (is (= id (:author-id (:value (schema/conform book {:title "x" :author {:href (str "/authors/" id)}} {})))))))

(deftest reports-a-link-it-cannot-read
  (is (= [:author] (mapv :field (:errors (schema/conform book {:title "x" :author "/authors/nope"} {}))))))

(deftest carries-a-curie
  (is (= "rel" (:curie (schema/parse config))))
  (is (= "x" (:curie (schema/parse (assoc config :curie "x")))))
  (is (false? (:curie (schema/parse (assoc config :curie false))))))

(deftest marks-a-relation-to-embed
  (let [model (schema/parse (assoc-in config [:resources :book :relations :author :embed] true))]
    (is (true? (get-in model [:resources :book :relations :author :embed?])))
    (is (false? (get-in model [:resources :author :relations :books :embed?])))))

(def described
  {:resources
   {:author {:fields {:id {:type :long :identity true} :name {:type :string}}}
    :book   {:fields      {:id        {:type :long :identity true}
                           :title     {:type :string}
                           :year      {:type :long}
                           :author-id {:type :long}}
             :relations   {:author {:kind :belongs-to :target :author :via :author-id}}
             :projections {:summary {:fields [:title]}
                           :full    {:fields [:title :year] :embed [:author]}}
             :searches    {:by-title {:predicates [:title]}
                           :by-year  {:predicates [:year]}}}}})

(deftest carries-templates-for-what-it-addresses
  (let [book (get-in (schema/parse described) [:resources :book])]
    (is (= "/books/{id}" (:self-template book)))
    (is (= "/profile/books" (:profile-path book)))
    (is (= "/books/search" (:search-path book)))))

(deftest carries-a-member-template-for-a-has-many-relation
  (let [model (schema/parse config)]
    (is (= "/authors/{id}/books/{book-key}"
           (get-in model [:resources :author :relations :books :member-template])))
    (is (nil? (get-in model [:resources :book :relations :author :member-template])))))

(deftest normalises-projections-and-searches
  (let [book (get-in (schema/parse described) [:resources :book])]
    (is (= [:title] (get-in book [:projections :summary :fields])))
    (is (= [:author] (get-in book [:projections :full :embed])))
    (is (= "/books/search/by-title" (get-in book [:searches :by-title :path])))
    (is (= "/books/search/by-title{?title,page,size,sort}"
           (get-in book [:searches :by-title :template])))))

(deftest rejects-a-projection-or-search-it-cannot-honour
  (is (thrown? clojure.lang.ExceptionInfo
               (schema/parse (assoc-in described [:resources :book :projections :summary :fields] [:nope]))))
  (is (thrown? clojure.lang.ExceptionInfo
               (schema/parse (assoc-in described [:resources :book :projections :summary :embed] [:nope]))))
  (is (thrown? clojure.lang.ExceptionInfo
               (schema/parse (assoc-in described [:resources :book :searches :by-title :predicates] [:nope])))))
