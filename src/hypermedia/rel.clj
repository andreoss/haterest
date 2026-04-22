(ns hypermedia.rel)

(def registered
  #{:self :first :prev :next :last :up :item :collection
    :profile :describedby :related :search :alternate :canonical})

(defn curied [curie relation]
  (if (and curie (not (contains? registered relation)))
    (keyword (str (name curie) ":" (name relation)))
    relation))

(defn curies [curie]
  (when curie
    [{:name (name curie) :href "/profile/{rel}" :templated true}]))
