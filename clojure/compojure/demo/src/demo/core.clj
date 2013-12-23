(ns demo.core
  (:use ;;webui.nav
  ;;  [clojure.java.io]
    [compojure core response]
    [ring.adapter.jetty :only [run-jetty]]
    [ring.util.response]
    [ring.middleware file file-info stacktrace reload])
  (:require [compojure.route :as route])
  (:gen-class))

(defroutes main-routes
  (GET "/" [] "root page")
  (route/not-found "foobar!");; (file "public/404.html"))
)

(defn app
  []
  (-> main-routes
 ;;     (wrap-reload '(demo.core view))
 ;;     (wrap-file "public")
      (wrap-file-info)
      (wrap-stacktrace)))

(defn start-server
  []
  (run-jetty (app) {:port 5000 :join? false}))

(defn -main [& args]
  (start-server))
