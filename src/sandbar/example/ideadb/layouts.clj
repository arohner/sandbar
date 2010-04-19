;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.example.ideadb.layouts
  (:require [clojure.contrib.str-utils2 :as s])
  (:use (hiccup core page-helpers)
        (sandbar [auth :only (current-user
                              current-username
                              any-role-granted?)]
                 stateful-session)
        (sandbar.dev html)
        (sandbar.example.ideadb data)))

(defn base-layout [title header request & body]
  (html
   (doctype :html4)
   [:html
    [:head
     [:meta {:http-equiv "X-UA-Compatible" :content "IE=EmulateIE7"}]
     [:title (str "Idea Database - " (s/capitalize title))]
     (icon "icon.png")
     (stylesheet "sandbar.css")
     (stylesheet "ideadb.css")]
    [:body
     [:div {:id "page"}
      [:table {:align "center" :cellpadding "0" :cellspacing "0"}
       [:tr
        [:td (if (any-role-granted? request :admin)
               {:align :right}
               {:align :center})
         header]]
       [:tr
        [:td
         [:div {:id "rounded-border-content"}
          [:div {:class "border-top"}
           [:div {:class "border-bottom"}
            [:div {:class "border-left"}
             [:div {:class "border-right"}
              [:div {:class "border-bottom-left-corner"}
               [:div {:class "border-bottom-right-corner"}
                [:div {:class "border-top-left-corner"}
                 [:div {:class "border-top-right-corner"}
                  [:div {:id "content"}
                   (if-let [m (get-flash-value! :user-message)]
                     [:div {:class "message"}
                      m])
                   body]]]]]]]]]]]]]
      [:div {:id "footer"}
          [:div "For questions on the idea database please contact the system
                 administrator at 555-5555."]
          (if (not (nil? (current-user)))
            [:div "You are currently logged in as "
           [:b (current-username)] ". "
           (clink-to "/logout" "logout")])]]
     (javascript "ideadb.js")]]))

(defn main-layout [title request & body]
  (base-layout title [:div {:style "padding-top:50px"}] request body))

(defn list-layout [title request & body]
  (base-layout title
               (if (any-role-granted? request :admin)
                 [:div {:id "idea-actions"}
                  (image-link "/idea/new"
                                   "submit_idea_off.png"
                                   "submit_idea_on.png" {})
                  (image-link "/admin/list"
                                   "edit_lists_off.png"
                                   "edit_lists_on.png" {})
                  (image-link "/idea/download"
                                   "download_off.png"
                                   "download_on.png" {})]
                 [:div {:id "idea-actions"}
                  (image-link "/idea/new"
                              "submit_idea_off.png"
                              "submit_idea_on.png" {})])
               request
               body))

(defn form-layout [title request & body]
  (base-layout title
               (if (any-role-granted? request :admin)
                 [:div {:id "form-page-header"}]
                 [:div {:id "form-page-header"}
                  (str "Welcome " (:name (current-user))
                       "! Use the form below to submit your idea.")])
               request
               body))

(defn admin-users-layout [title request & body]
  (base-layout title
               [:div {:id "form-page-header"}]
               request
               (if (.endsWith (:uri request) "list")
                 [:div
                  [:div (clink-to "/admin/list" "Return to Edit Lists")]
                  [:br]
                  body]
                 body)))





