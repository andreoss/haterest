(ns smoke)
(require '[hypermedia.main :as main] '[clojure.string :as str])
(import '(java.net URI) '(java.net.http HttpClient HttpClient$Version HttpRequest
                                        HttpRequest$BodyPublishers HttpResponse$BodyHandlers)
        '(java.time Duration))

(def client (-> (HttpClient/newBuilder) (.version HttpClient$Version/HTTP_1_1)
                (.connectTimeout (Duration/ofSeconds 2)) (.build)))

(defn call [port method path & {:keys [body content-type accept headers]}]
  (let [publisher (if body (HttpRequest$BodyPublishers/ofString body) (HttpRequest$BodyPublishers/noBody))
        builder (cond-> (-> (HttpRequest/newBuilder (URI/create (str "http://127.0.0.1:" port path)))
                            (.timeout (Duration/ofSeconds 3))
                            (.header "Connection" "close")
                            (.method (str/upper-case (name method)) publisher))
                  content-type (.header "Content-Type" content-type)
                  accept (.header "Accept" accept))
        builder (reduce (fn [b [k v]] (.header b k v)) builder headers)
        response (.send client (.build builder) (HttpResponse$BodyHandlers/ofString))]
    {:status (.statusCode response)
     :location (.orElse (.firstValue (.headers response) "location") nil)
     :etag (.orElse (.firstValue (.headers response) "etag") nil)
     :allow (.orElse (.firstValue (.headers response) "allow") nil)
     :type (.orElse (.firstValue (.headers response) "content-type") nil)
     :body (.body response)}))

(defn show [label response]
  (println (format "%-46s %s %s" label (:status response)
                   (let [b (:body response)] (if (> (count b) 210) (str (subs b 0 210) " ...") b)))))

(let [running (main/start {:schema "example.edn"
                           :database "jdbc:h2:mem:smoke;DB_CLOSE_DELAY=-1"
                           :port 0 :migrate true})
      port (:port running)]
  (try
    (println "bound to port" port)
    (show "GET /" (call port :get "/"))
    (let [author (call port :post "/authors" :body "{\"name\":\"Herbert\"}" :content-type "application/json")
          book   (call port :post "/books"
                       :body (str "{\"title\":\"Dune\",\"year\":1965,\"author\":\"" (:location author) "\"}")
                       :content-type "application/json")]
      (show "POST /authors" author)
      (show "POST /books" book)
      (show "GET  book" (call port :get (:location book)))
      (show "GET  book?projection=summary" (call port :get (str (:location book) "?projection=summary")))
      (show "GET  book/author" (call port :get (str (:location book) "/author")))
      (show "GET  /books/search" (call port :get "/books/search"))
      (show "GET  /books/search/by-title?title=Dune" (call port :get "/books/search/by-title?title=Dune"))
      (show "GET  /profile/books (alps)" (call port :get "/profile/books" :accept "application/alps+json"))
      (show "GET  book (hal-forms)" (call port :get (:location book) :accept "application/prs.hal-forms+json"))
      (println (format "%-46s %s" "OPTIONS book" (:allow (call port :options (:location book)))))
      (let [tag (:etag (call port :get (:location book)))]
        (println (format "%-46s %s" "ETag" tag))
        (show "GET  book If-None-Match" (call port :get (:location book) :headers {"If-None-Match" tag}))
        (show "PATCH book If-Match stale" (call port :patch (:location book)
                                                :body "{\"year\":1966}" :content-type "application/json"
                                                :headers {"If-Match" "\"stale\""}))
        (show "PATCH book If-Match current" (call port :patch (:location book)
                                                  :body "{\"year\":1966}" :content-type "application/json"
                                                  :headers {"If-Match" tag})))
      (show "GET  /health" (call port :get "/health"))
      (show "DELETE book" (call port :delete (:location book)))
      (show "GET  book after removal" (call port :get (:location book))))
    (finally ((:stop running)))))
