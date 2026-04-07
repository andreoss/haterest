(ns hypermedia.store)

(defprotocol Store
  (fetch [this resource id])
  (query [this resource criteria]))
