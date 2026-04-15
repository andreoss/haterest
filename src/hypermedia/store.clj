(ns hypermedia.store)

(defprotocol Store
  (fetch [this resource id])
  (query [this resource criteria])
  (total [this resource criteria])
  (probe [this])
  (create! [this resource row])
  (replace! [this resource id row])
  (amend! [this resource id row])
  (erase! [this resource id])
  (transact [this body]))
