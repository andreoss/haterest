(ns hypermedia.store)

(defprotocol Store
  (fetch [this resource id])
  (query [this resource criteria])
  (total [this resource criteria])
  (probe [this])
  (create! [this resource row])
  (replace! [this resource id row expected])
  (amend! [this resource id row expected])
  (erase! [this resource id expected])
  (transact [this body])
  (linked [this owner target relation owner-id criteria])
  (linked-total [this owner target relation owner-id])
  (links-of [this owner target relation owner-ids])
  (link! [this owner target relation owner-id target-ids])
  (unlink! [this owner target relation owner-id target-ids]))

(def version-key :hypermedia/version)

(defn version-of [row] (or (get row version-key) 0))
