(ns hypermedia.store.counting
  (:require [hypermedia.store :as store]))

(defrecord Counting [inner tally]
  store/Store
  (fetch [_ resource id] (swap! tally update :fetch (fnil inc 0)) (store/fetch inner resource id))
  (query [_ resource criteria] (swap! tally update :query (fnil inc 0)) (store/query inner resource criteria))
  (total [_ resource criteria] (swap! tally update :total (fnil inc 0)) (store/total inner resource criteria))
  (probe [_] (store/probe inner))
  (create! [_ resource row] (store/create! inner resource row))
  (replace! [_ resource id row] (store/replace! inner resource id row))
  (amend! [_ resource id row] (swap! tally update :amend (fnil inc 0)) (store/amend! inner resource id row))
  (erase! [_ resource id] (store/erase! inner resource id))
  (transact [this body] (body this)))

(defn counting [inner]
  (->Counting inner (atom {})))

(defn tally [subject] @(:tally subject))

(defn reset-tally [subject] (reset! (:tally subject) {}))
