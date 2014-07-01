(ns html-gen.views.welcome
  (:require [html-gen.views.common :as common]
            [noir.content.getting-started])
  (:use [noir.core :only [defpage defpartial]]))

(use 'hiccup.core)
(use 'hiccup.form)
(use 'hiccup.element)

(defpage "/welcome" []
  (common/layout
   [:p "bar"]))

(defpartial post-item [{:keys [perma-link title md-body date tme]}]
		  [:li
		  [:h2 (link-to perma-link title)]
		  [:u1
		  [:li date]
		  [:li tme]]
                   [:div md-body]])

(defpartial posts-list [items]
  [:u1
   (map post-item items)])

(defpage "/posts" []
  (common/layout
   (posts-list [{:perma-link "www.danielgrigg.com/foo123"
                 :title "foo"
                 :md-body "Foo rules!"
                 :date "27-05-12"
                 :tme "12:12"}])))
