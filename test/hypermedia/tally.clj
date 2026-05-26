(ns hypermedia.tally
  (:import (java.lang.reflect InvocationHandler Method Proxy)
           (java.sql Connection)
           (javax.sql DataSource)))

(def counted #{"prepareStatement" "prepareCall" "createStatement"})

(defn- handler-for [target statements]
  (reify InvocationHandler
    (invoke [_ _proxy method args]
      (let [^Method method method]
        (when (contains? counted (.getName method))
          (swap! statements inc))
        (try (.invoke method target (if args (object-array args) (object-array 0)))
             (catch java.lang.reflect.InvocationTargetException e
               (throw (.getCause e))))))))

(defn- proxied [^Class interface target statements]
  (Proxy/newProxyInstance (.getClassLoader interface)
                          (into-array Class [interface])
                          (handler-for target statements)))

(defn watching
  [^DataSource inner]
  (let [statements (atom 0)]
    {:statements statements
     :datasource
     (reify DataSource
       (getConnection [_]
         (proxied Connection (.getConnection inner) statements))
       (getConnection [_ user password]
         (proxied Connection (.getConnection inner user password) statements))
       (getLoginTimeout [_] (.getLoginTimeout inner))
       (setLoginTimeout [_ seconds] (.setLoginTimeout inner seconds))
       (getLogWriter [_] (.getLogWriter inner))
       (setLogWriter [_ writer] (.setLogWriter inner writer))
       (getParentLogger [_] (.getParentLogger inner))
       (isWrapperFor [_ iface] (.isWrapperFor inner iface))
       (unwrap [_ iface] (.unwrap inner iface)))}))

(defn reset! [watched] (clojure.core/reset! (:statements watched) 0))

(defn count-of [watched] @(:statements watched))
