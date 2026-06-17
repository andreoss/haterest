(ns hypermedia.server
  (:require [ring.adapter.jetty :as jetty])
  (:import (org.eclipse.jetty.server Server ServerConnector)
           (org.eclipse.jetty.server.handler StatisticsHandler)))

(def default-drain 10000)

(defn- draining [^Server server ^long drain]
  (let [counted (StatisticsHandler.)]
    (.setHandler counted (.getHandler server))
    (.setHandler server counted)
    (.setStopTimeout server drain)))

(defn start [handler {:keys [port host drain]
                      :or   {port 0 host "127.0.0.1" drain default-drain}}]
  (let [^Server jetty (jetty/run-jetty handler {:port port :host host :join? false
                                                :configurator #(draining % drain)})
        ^ServerConnector connector (first (.getConnectors jetty))]
    {:jetty jetty
     :host  host
     :port  (.getLocalPort connector)
     :drain drain
     :stop  (fn [] (.stop jetty))}))
