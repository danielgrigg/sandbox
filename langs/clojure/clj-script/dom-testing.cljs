(ns countdownd.hello
  (:require 
   [clojure.browser.dom :as dom]))
;; [clojure.browser.event :as event]

(dom/append (dom/get-element "content")
            (dom/element "Hello World again!"))
(dom/append (dom/get-element "content")
            (dom/element "Hello World mk2!"))
(dom/append (dom/get-element "content")
            (dom/element :div "divider"))
(dom/set-text :header "setting-text-here")