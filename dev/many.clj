(ns many)
(require '[hypermedia.main :as main] '[clojure.string :as str])
(import '(java.net URI) '(java.net.http HttpClient HttpClient$Version HttpRequest
                                        HttpRequest$BodyPublishers HttpResponse$BodyHandlers)
        '(java.time Duration))

(def client (-> (HttpClient/newBuilder) (.version HttpClient$Version/HTTP_1_1)
                (.connectTimeout (Duration/ofSeconds 2)) (.build)))

(defn call [port method path & {:keys [body content-type]}]
  (let [publisher (if body (HttpRequest$BodyPublishers/ofString body) (HttpRequest$BodyPublishers/noBody))
        builder (cond-> (-> (HttpRequest/newBuilder (URI/create (str "http://127.0.0.1:" port path)))
                            (.timeout (Duration/ofSeconds 3))
                            (.header "Connection" "close")
                            (.method (str/upper-case (name method)) publisher))
                  content-type (.header "Content-Type" content-type))
        response (.send client (.build builder) (HttpResponse$BodyHandlers/ofString))]
    {:status (.statusCode response)
     :location (.orElse (.firstValue (.headers response) "location") nil)
     :allow (.orElse (.firstValue (.headers response) "allow") nil)
     :body (.body response)}))

(defn show [label r]
  (println (format "%-44s %s %s" label (:status r)
                   (let [b (:body r)] (if (> (count b) 230) (str (subs b 0 230) " ...") b)))))

(let [running (main/start {:schema "catalogue.edn" :database "jdbc:h2:mem:many;DB_CLOSE_DELAY=-1"
                           :port 0 :migrate true})
      port (:port running)]
  (try
    (let [herbert (call port :post "/authors" :body "{\"name\":\"Herbert\"}" :content-type "application/json")
          anderson (call port :post "/authors" :body "{\"name\":\"Anderson\"}" :content-type "application/json")
          dune (call port :post "/books" :body "{\"title\":\"Dune\",\"year\":1965}" :content-type "application/json")
          sandworms (call port :post "/books" :body "{\"title\":\"Sandworms\",\"year\":2007}" :content-type "application/json")]
      (show "GET  /" (call port :get "/"))
      (println (format "%-44s %s" "OPTIONS author books"
                       (:allow (call port :options (str (:location herbert) "/books")))))
      (show "POST herbert/books <- dune, sandworms"
            (call port :post (str (:location herbert) "/books")
                  :body (str (:location dune) "\n" (:location sandworms)) :content-type "text/uri-list"))
      (show "POST anderson/books <- sandworms"
            (call port :post (str (:location anderson) "/books")
                  :body (:location sandworms) :content-type "text/uri-list"))
      (show "GET  herbert/books" (call port :get (str (:location herbert) "/books?sort=title,asc")))
      (show "GET  sandworms/authors" (call port :get (str (:location sandworms) "/authors")))
      (show "GET  /authors (embedded)" (call port :get "/authors"))
      (show "GET  herbert/books slice" (call port :get (str (:location herbert) "/books?size=1&sort=title,asc")))
      (show "DELETE herbert/books/<sandworms>"
            (call port :delete (str (:location herbert) "/books/"
                                    (last (str/split (:location sandworms) #"/")))))
      (show "GET  herbert/books after removal" (call port :get (str (:location herbert) "/books")))
      (show "PUT  herbert/books <- sandworms only"
            (call port :put (str (:location herbert) "/books")
                  :body (:location sandworms) :content-type "text/uri-list"))
      (show "GET  herbert/books" (call port :get (str (:location herbert) "/books")))
      (show "GET  dune/authors" (call port :get (str (:location dune) "/authors")))
      (show "GET  /profile/authors" (call port :get "/profile/authors")))
    (finally ((:stop running)))))
