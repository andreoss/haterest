(ns hypermedia.e2e-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hypermedia.client :as client]
            [hypermedia.database :as database]
            [hypermedia.main :as main]))

(defn- serving [engine schema body]
  (let [running (main/start {:schema schema :database (:url engine) :port 0 :migrate true})]
    (try (body (:port running)) (finally ((:stop running))))))

(defn- titles [body key] (mapv :title (get-in body [:_embedded key])))

(defn- names [body key] (mapv :name (get-in body [:_embedded key])))

(defn- create [port path payload]
  (client/request port :post path :body (client/json-body payload) :content-type "application/json"))

(defn- simple-scenario [engine]
  (serving
   engine "e2e-simple.edn"
   (fn [port]
     (testing "the root offers its collections"
       (let [root (:body (client/request port :get "/"))]
         (is (= "/writers" (get-in root [:_links :rel:writers :href])))
         (is (= "/works" (get-in root [:_links :rel:works :href])))
         (is (= "/profile" (get-in root [:_links :profile :href])))))

     (let [writer (create port "/writers" {:name "Herbert"})
           other  (create port "/writers" {:name "Austen"})]
       (is (= 201 (:status writer)))
       (is (some? (:location writer)))

       (testing "a resource is created against a link it was given"
         (let [work (create port "/works" {:title "Dune" :year 1965 :writer (:location writer)})]
           (is (= 201 (:status work)))
           (is (= "Herbert" (get-in work [:body :_embedded :writer :name])))
           (is (= "Herbert" (get-in (client/request port :get (get-in work [:body :_links :rel:writer :href]))
                                    [:body :name])))))

       (create port "/works" {:title "Messiah" :year 1969 :writer (:location writer)})
       (create port "/works" {:title "Emma" :year 1815 :writer (:location other)})

       (testing "a collection is a walkable slice"
         (loop [href "/works?size=2&sort=title,asc" seen [] guard 0]
           (let [body (:body (client/request port :get href))
                 seen (into seen (titles body :works))]
             (if-let [next (get-in body [:_links :next :href])]
               (if (< guard 5) (recur next seen (inc guard)) (is false "the walk did not end"))
               (is (= ["Dune" "Emma" "Messiah"] seen))))))

       (testing "an association is a slice of its own"
         (let [owned (:body (client/request port :get (get-in writer [:body :_links :rel:works :href])))]
           (is (= 2 (get-in owned [:page :totalElements])))
           (is (= ["Dune" "Messiah"] (sort (titles owned :works))))))

       (testing "a search answers from the store"
         (let [found (:body (client/request port :get "/works/search/by-title?title=Emma"))]
           (is (= ["Emma"] (titles found :works))))
         (let [found (:body (client/request port :get "/works/search/by-year?year=1965"))]
           (is (= ["Dune"] (titles found :works)))))

       (testing "a projection narrows a document"
         (let [work (first (get-in (client/request port :get "/works?sort=title,asc") [:body :_embedded :works]))
               self (get-in work [:_links :self :href])
               thin (:body (client/request port :get (str self "?projection=summary")))]
           (is (= "Dune" (:title thin)))
           (is (not (contains? thin :year)))))

       (testing "a stale write is refused and a current one is taken"
         (let [work    (first (get-in (client/request port :get "/works?sort=title,asc") [:body :_embedded :works]))
               self    (get-in work [:_links :self :href])
               tag     (:etag (client/request port :get self))
               fresh   (client/request port :get self :headers {"If-None-Match" tag})
               amended (client/request port :patch self
                                       :body (client/json-body {:year 1966})
                                       :content-type "application/json"
                                       :headers {"If-Match" tag})]
           (is (= 304 (:status fresh)))
           (is (= 200 (:status amended)))
           (is (= 1966 (get-in amended [:body :year])))
           (is (= 412 (:status (client/request port :patch self
                                               :body (client/json-body {:year 1967})
                                               :content-type "application/json"
                                               :headers {"If-Match" tag}))))
           (is (= 204 (:status (client/request port :delete self))))
           (is (= 404 (:status (client/request port :get self))))))

       (testing "a submission that does not fit is a problem document"
         (let [refused (create port "/works" {:year 2000})]
           (is (= 422 (:status refused)))
           (is (str/starts-with? (:type refused) "application/problem+json"))
           (is (= "title" (get-in refused [:body :errors 0 :field])))))))))

(defn- many-scenario [engine]
  (serving
   engine "e2e-many.edn"
   (fn [port]
     (let [ada    (create port "/players" {:name "Ada"})
           grace  (create port "/players" {:name "Grace"})
           blue   (create port "/teams" {:name "Blue"})
           red    (create port "/teams" {:name "Red"})
           roster (get-in ada [:body :_links :rel:teams :href])]

       (testing "links are written with uri-list"
         (is (= 204 (:status (client/request port :post roster
                                             :body (str (:location blue) "\n" (:location red))
                                             :content-type "text/uri-list"))))
         (is (= 204 (:status (client/request port :post (get-in grace [:body :_links :rel:teams :href])
                                             :body (:location red)
                                             :content-type "text/uri-list")))))

       (testing "both sides traverse the same join"
         (is (= ["Blue" "Red"] (sort (names (:body (client/request port :get roster)) :teams))))
         (is (= ["Ada" "Grace"]
                (sort (names (:body (client/request port :get (str (:location red) "/players"))) :players))))
         (is (= ["Ada"]
                (names (:body (client/request port :get (str (:location blue) "/players"))) :players))))

       (testing "a traversal slices and sorts"
         (let [slice (:body (client/request port :get (str roster "?size=1&sort=name,desc")))]
           (is (= ["Red"] (names slice :teams)))
           (is (= 2 (get-in slice [:page :totalElements])))))

       (testing "a page embeds through the join"
         (let [page (:body (client/request port :get "/players?sort=name,asc"))]
           (is (= [["Blue" "Red"] ["Red"]]
                  (map #(sort (map :name (get-in % [:_embedded :teams])))
                       (get-in page [:_embedded :players]))))))

       (testing "one member is taken out and the set replaced"
         (is (= 204 (:status (client/request port :delete
                                             (str roster "/" (last (str/split (:location blue) #"/")))))))
         (is (= ["Red"] (names (:body (client/request port :get roster)) :teams)))
         (is (= 204 (:status (client/request port :put roster
                                             :body (:location blue)
                                             :content-type "text/uri-list"))))
         (is (= ["Blue"] (names (:body (client/request port :get roster)) :teams)))
         (is (= ["Grace"]
                (names (:body (client/request port :get (str (:location red) "/players"))) :players))))

       (testing "a reference of the wrong kind is refused"
         (is (= 422 (:status (client/request port :put roster
                                             :body (:location ada)
                                             :content-type "text/uri-list")))))))))

(def shaped
  {:label  "a label"
   :note   "a longer note that lives in a text column"
   :tally  42
   :ratio  0.5
   :amount 19.99
   :done   true
   :moment "2026-09-25T10:00:00Z"
   :day    "2026-09-25"
   :order  7})

(defn- shapes-scenario [engine]
  (serving
   engine "e2e-shapes.edn"
   (fn [port]
     (let [created (create port "/records" shaped)]
       (is (= 201 (:status created)))
       (testing "every declared type survives the round trip"
         (let [read-back (:body (client/request port :get (:location created)))]
           (is (= "a label" (:label read-back)))
           (is (= (:note shaped) (:note read-back)))
           (is (= 42 (:tally read-back)))
           (is (= 0.5 (:ratio read-back)))
           (is (= 19.99 (double (:amount read-back))))
           (is (true? (:done read-back)))
           (is (= "2026-09-25T10:00:00Z" (:moment read-back)))
           (is (= "2026-09-25" (:day read-back)))
           (is (= 7 (:order read-back)))))
       (testing "a reserved word is still a usable field"
         (let [sorted (:body (client/request port :get "/records?sort=order,desc"))]
           (is (= 1 (get-in sorted [:page :totalElements])))))
       (testing "a boolean survives an amendment"
         (let [amended (client/request port :patch (:location created)
                                       :body (client/json-body {:done false :tally 43})
                                       :content-type "application/json")]
           (is (= false (get-in amended [:body :done])))
           (is (= 43 (get-in amended [:body :tally])))))))))

(defn- walked [port href limit]
  (loop [href href seen [] guard 0]
    (let [body (:body (client/request port :get href))
          seen (into seen (titles body :works))]
      (if (and (get-in body [:_links :next :href]) (< guard limit))
        (recur (get-in body [:_links :next :href]) seen (inc guard))
        seen))))

(defn- walking-scenario [engine]
  (serving
   engine "e2e-simple.edn"
   (fn [port]
     (let [writer (create port "/writers" {:name "Many"})]
       (doseq [t (mapv #(format "work %02d" %) (range 25))]
         (create port "/works" {:title t :year 2000 :writer (:location writer)}))
       (let [everything (titles (:body (client/request port :get "/works?size=200&sort=title,asc"))
                                :works)]

         (testing "following next reaches every row once, in order"
           (let [seen (walked port "/works?size=7&sort=title,asc" 20)]
             (is (= everything seen))
             (is (= (count seen) (count (distinct seen))))))

         (testing "the same walk downwards"
           (is (= (reverse everything) (walked port "/works?size=7&sort=title,desc" 20))))

         (testing "and with no ordering asked for"
           (let [seen (walked port "/works?size=7" 20)]
             (is (= (count everything) (count seen)))
             (is (= (count seen) (count (distinct seen))))))

         (testing "a cursor from one ordering is refused by another"
           (let [href  (get-in (client/request port :get "/works?size=7&sort=title,asc")
                               [:body :_links :next :href])
                 moved (str/replace href "sort=title%2Casc" "sort=year%2Casc")]
             (is (= 400 (:status (client/request port :get moved)))))))))))

(def scenarios
  [["navigation and writes" simple-scenario]
   ["many to many"          many-scenario]
   ["declared value types"  shapes-scenario]
   ["walking by cursor"     walking-scenario]])

(deftest every-engine-serves-the-same-api
  (let [{:keys [ready declined]} (database/engines)]
    (.println System/err (str "e2e engines ready: " (str/join ", " (map (comp name :name) ready))))
    (when (seq declined)
      (.println System/err (str "e2e engines declined: " (str/join ", " (map first declined)))))
    (is (seq ready) "no database engine was available")
    (is (contains? (set (map :name ready)) :h2) "the embedded engine must always run")
    (doseq [engine ready]
      (try
        (doseq [[label scenario] scenarios]
          (testing (str (name (:name engine)) " / " label)
            (scenario engine)))
        (finally ((:stop engine)))))))
