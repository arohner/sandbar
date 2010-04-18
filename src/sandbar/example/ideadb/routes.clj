;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.example.ideadb.routes
  (:use (compojure core)
        (ring.util [response :only (redirect)])
        (sandbar [core :only (cpath)] )
        (sandbar.dev html
                     forms
                     userui
                     standard-pages
                     list-manager
                     basic-authentication
                     autorouter)
        (sandbar.example.ideadb view
                                properties
                                layouts
                                model
                                user-model)))

;; Reduce 117 lines to routing code to _ using autorouter
;; 3024 total lines of code...

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
    (cond (or (= a "list") (= a "download"))
          [c (str c "-" a)] 
          (or (= a "new")
              (= a "edit")
              (= a "delete"))
          [c (str a "-" c)] )
    (if (= a "ideas")
      ["idea" "idea-list"]
      [c a])))

(defroutes user-module-routes
  (autorouter route-adapter))

(defroutes admin-module-routes
  (GET "/admin/list*" request
       (main-layout "Administrator"
                    request
                    (admin-menu request)))
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
