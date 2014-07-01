(ns forms.views.welcome
  (:require [forms.views.common :as common]
            [noir.content.getting-started]
            [noir.response :as resp]
            [noir.validation :as vali]
            [noir.session :as session])
  
  (:use [noir.core :only [defpage defpartial render]]
        ))

(use 'hiccup.form)

(defpartial error-item [[first-error]]
  [:p.error first-error])

(defpartial user-fields [{:keys [firstname lastname]}]
  (vali/on-error :firstname error-item)
  (label "firstname" "First name")
  (text-field "firstname" firstname)
  (vali/on-error :lastname error-item)
  (label "lastname" "Last name")
  (text-field "lastname" lastname))

(defpage "/user/add" {:as user}
  (common/layout
   (form-to [:post "/user/add"]
            (user-fields user)
            (submit-button "Add user"))))


(defn valid? [{:keys [firstname lastname]}]
  (vali/rule (vali/min-length? firstname 5)
             [:firstname "firstname must be > 5 letters"])
  (vali/rule (vali/has-value? lastname)
             [:lastname "must have last name"])
  (not (vali/errors? :lastname :firstname)))

(defpage [:post "/user/add"] {:as user}
  (if (valid? user)
    (common/layout
     [:p "added user"])
     (render "/user/add" user)))
