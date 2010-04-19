;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.example.ideadb.control
  (:use (compojure core)
        (ring.util [response :only (redirect)])
        (sandbar auth
                 [core :only (cpath)] )
        (sandbar.dev html
                     forms
                     userui
                     standard-pages
                     list-manager
                     basic-authentication
                     autorouter)
        (sandbar.example.ideadb properties
                                layouts
                                data
                                users)))

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
               "category" "Edit Categories"
               "status" "Edit Status List"
               "type" "Edit Types"}]
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
    (configure-database request)
    (handler request)))

(def my-list-editor
     (partial list-editor paged-list find-by-id properties
              (cpath "/admin/list")))
(def my-list-updater (partial list-updater save delete-by-id))

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

(defroutes user-module-routes
  (autorouter route-adapter))

(defroutes admin-module-routes
  (GET "/admin/list*" request
       (main-layout "Administrator"
                    request
                    (admin-menu-view request)))
  
  (GET "/admin/business-unit*" request
       (main-layout "Edit Business Units"
                    request
                    (my-list-editor :business_unit request (:params request))))
  (POST "/admin/business-unit*" {params :params}
        (my-list-updater :business_unit params))
  
  (GET "/admin/category*" request
       (main-layout "Edit Categories"
                    request
                    (my-list-editor :idea_category request (:params request))))
  (POST "/admin/category*" {params :params}
        (my-list-updater :idea_category params))
  
  (GET "/admin/status*" request
       (main-layout "Edit Status List"
                    request
                    (my-list-editor :idea_status request (:params request))))
  (POST "/admin/status*" {params :params}
        (my-list-updater :idea_status params))
  
  (GET "/admin/type*" request
       (main-layout "Edit Types"
                    request
                    (my-list-editor :idea_type request (:params request))))
  (POST "/admin/type*" {params :params}
        (my-list-updater :idea_type params))
  
  (security-edit-user-routes "/admin" (var admin-users-layout) (fn [r] (:uri r))
                             properties user-data-functions)
  (basic-auth-routes (fn [r & b] (main-layout "Login" r b)) 
                     properties (UserModel (user-data-functions :load))))
