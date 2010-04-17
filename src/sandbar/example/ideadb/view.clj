;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.example.ideadb.view
  (:use (hiccup core)
        (sandbar core
                 [auth :only (current-username
                              current-user-roles
                              any-role-granted?)]
                 stateful-session)
        (sandbar.dev tables forms html util validation)
        (sandbar.example.ideadb model)))

(defn index []
  [:div [:h3 "Welcome!"]
   [:p "This is a sample application that uses many of the features of the
         sandbar library. The application is a database for ideas. Imagine
         that it is being used at a company to collect ideas from employees.
         Administrators can view all ideas and edit them. Users can only add
         ideas and view the ones that they have already added."] 
    [:br] [:br]
    [:div "User : " (current-username)]
    [:div "Role : " (current-user-roles)]
    [:br] [:br]
    (clink-to "/idea/list" "Idea List")])

(defn admin-menu [request]
  (let [links {"business-unit" "Edit Business Units"
               "category" "Edit Categories"
               "status" "Edit Status List"
               "type" "Edit Types"}]
    [:div
     [:div (clink-to "/idea/list" "Return to Idea list")]
     [:br]
     [:div (clink-to "/admin/user/list" "Edit Users")]
     [:br]
     (map #(vector :div (clink-to (str "/admin/" (key %) "/list") (val %)))
         links)]))
