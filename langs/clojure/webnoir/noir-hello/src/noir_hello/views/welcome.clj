(ns noir-hello.views.welcome
  (:require [noir-hello.views.common :as common]
            [noir.content.getting-started]
            [noir.response])
  (:use [noir.core :only [defpage defpartial]]
        [hiccup.page :only [include-css html5]]))


(defpage "/welcome" []
         (common/layout
           [:p "Welcome to noir-hello"]))

(defpage "/my-page" []
  (html5
   [:h1 "this is my first page"]))

(defpartial todo-item [{:keys [id title due]}]
  [:li {:id id}
   [:h3 title]
   [:span.due due]])

(defpartial todos-list [items]
  [:ul#todoItems
   (map todo-item items)])

(def todo-db (ref {}))
(def todo-next-id (ref 0))
(defn reset-db []
  (dosync
   (ref-set todo-db {})
   (ref-set todo-next-id 0)))

(defn all-todos [] (vals @todo-db))

(defn add-todo [title due]
  (dosync
   (alter todo-next-id inc)
   (let [id (str "todo" @todo-next-id)]
     (alter todo-db assoc
            (keyword id) {:id id :title title :due due})
     @todo-next-id)))

(defn remove-todo [id]
  (dosync
   (alter todo-db dissoc id)))
            
(defpage [:post "/todos"] {:keys [title due]}
  (if-let [todo-id (add-todo title due)]
    (noir.response/json {:id todo-id
                    :title title
                    :due-date due})
    (noir.response/empty)))

(defpage "/todos" {}
  (let [items (all-todos)]
    (common/layout
     [:h1 "Todo list"]
     (todos-list items))))

;; dynamic url, ie, curl -d "" localhost:8080/foo/23
(defpage [:post "/foo/:id"] {id :id}
  (common/layout
   [:p (str "foo id: " id)]))

;; keyed post, ie, curl -d "username=foo&password=bar localhost:8080/login
(defpage [:post "/login"] {:keys [username password]}
  (str "login as " username " with password " password))
