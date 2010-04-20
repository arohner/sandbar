;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns example.ideadb.control
  (:use (compojure core)
        (ring.util [response :only (redirect)])
        (sandbar auth
                 [core :only (cpath
                              clink-to
                              property-lookup)]
                 user-manager
                 standard-pages
                 list-manager
                 basic-authentication
                 autorouter
                 util)
        (example.ideadb properties
                        layouts
                        users))
  (:require (example.ideadb [data :as data])))

;;
;; Views
;; =====
;;

(defn index-view []
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
    (clink-to "/ideas" "Idea List")])

(defn admin-menu-view [request]
  (let [links {"business-unit" "Edit Business Units"
               "idea-category" "Edit Categories"
               "idea-status" "Edit Status List"
               "idea-type" "Edit Types"}]
    [:div
     [:div (clink-to "/ideas" "Return to Idea list")]
     [:br]
     [:div (clink-to "/admin/user/list" "Edit Users")]
     [:br]
     (map #(vector :div (clink-to (str "/admin/" (key %) "/list") (val %)))
         links)]))

;;
;; Control
;; =======
;;

(defn index [request]
  (main-layout "Home" request (index-view)))

(defn permission-denied [request]
  (main-layout "Permission Denied"
               request
               (permission-denied-page)))

;;
;; Routes
;; ======
;;

(defn with-db-configured [handler]
  (fn [request]
    (data/configure-database request)
    (handler request)))

(defn route-adapter
  "Adapt the routing algorithm to this project."
  [c a]
  (if c
    (let [ctrl (if (= c "idea") "ideas" c)]
        (cond (or (= a "list") (= a "download"))
           [ctrl (str c "-" a)] 
           (or (= a "new")
               (= a "edit")
               (= a "delete"))
           [ctrl (str a "-" c)]))
    (if (= a "ideas")
      ["ideas" "idea-list"]
      [c a])))

(defn simple-list [type properties]
  {:paged-list (fn [filters] (data/paged-list type filters))
   :find-by-id (fn [id] (data/find-by-id type id))
   :save (fn [m] (data/save m))
   :delete-by-id (fn [id] (data/delete-by-id type id))
   :visible-name (property-lookup properties type)
   :id type
   :properties properties})

(defroutes ideadb-routes
  (autorouter route-adapter)
  
  (GET "/admin/list*" request
       (main-layout "Administrator"
                    request
                    (admin-menu-view request)))
  
  (apply routes
         (map #(list-manager-routes
                (fn [r b]
                  (main-layout (str "Edit "
                                    (property-lookup properties  %)
                                    " List")
                               r
                               b))
                "/admin"
                (cpath "/admin/list")
                (simple-list % properties))
              [:business_unit :idea_category :idea_status :idea_type]))
  
  (security-edit-user-routes "/admin" (var admin-users-layout) (fn [r] (:uri r))
                             properties user-data-functions)
  
  (basic-auth-routes (fn [r & b] (main-layout "Login" r b)) 
                     properties (user-model (user-data-functions :load))))
