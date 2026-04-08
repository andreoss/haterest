(ns hypermedia.server
  (:require [ring.adapter.jetty :as jetty])
  (:import (org.eclipse.jetty.server Server ServerConnector)))

(defn start [handler {:keys [port host] :or {port 0 host "127.0.0.1"}}]
  (let [^Server jetty (jetty/run-jetty handler {:port port :host host :join? false})
        ^ServerConnector connector (first (.getConnectors jetty))]
    {:jetty jetty
     :host  host
     :port  (.getLocalPort connector)
     :stop  (fn [] (.stop jetty))}))
