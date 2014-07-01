(ns hello_cljs.views
  (:require
    [hiccup
      [page :refer [html5]]
      [element :refer [javascript-tag]]
      [page :refer [include-js]]]))

(defn- include-clojurescript [path]
  (list
    (javascript-tag "var CLOSURE_NO_DEPS = true;")
    (include-js path)))

(defn index-page []
  (html5
    [:head
      [:title " hello-cljs"]
      (include-clojurescript "/js/main.js")]
    [:body
      [:h1 "Hello from hello-cljs"]]))

