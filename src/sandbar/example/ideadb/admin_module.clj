; Copyright (c) Brenton Ashworth. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file COPYING at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns sandbar.example.ideadb.admin_module
  (:use (hiccup core)
        (compojure core)
        (ring.util [response :only (redirect)])
        (sandbar library auth basic_authentication userui)
        (sandbar.example.ideadb
         [layouts :only (main-layout form-layout admin-users-layout)]
         [model]
         [user_module])))

(defn edit-idea-form [request params]
  (let [form-data (find-by-id :idea (get params "id"))]
    (standard-form "Administrator Form" "/idea/edit"
                   "Save Changes"
                   (form-layout-grid [1 1 1 1 4]
                                     :idea
                                     (conj
                                      (concat (public-idea-fields)
                                              (admin-idea-fields))
                                      (form-hidden :id)
                                      (form-hidden :date_entered)
                                      (form-hidden :user_id))
                                     request
                                     form-data))))
 
(defn download-ideas []
  (let [data (filter-and-sort-records :idea {} {})
        fields [:id :name :description :customer_need :originator
                :date_entered :category :idea_type :business_unit :status]
        data (concat [(map #(% properties) fields)]
                     (map #(map (fn [field] (% field)) fields) data))]
    (format-csv data)))

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

;;
;; Administrator Routes
;; ====================
;;

(def my-list-editor
     (partial list-editor paged-list find-by-id properties
              (cpath "/admin/list")))
(def my-list-updater (partial list-updater save delete-by-id))

(defn user-data-functions [k]
  (cond (= k :save)
        (fn [m]
          (if (= :app_user (:type m))
            (standard-save-user m
                                filter-and-sort-records
                                delete-rec
                                save)
            (save m)))
        (= k :load) filter-and-sort-records
        (= k :lookup)
        (fn [type id]
          (if (= :app_user type)
            (standard-lookup-user type id
                                  filter-and-sort-records
                                  find-by-id)
            (find-by-id type id)))
        (= k :delete)
        (fn [type id]
          (if (= :app_user type)
            (standard-delete-user type id
                                  filter-and-sort-records
                                  delete-by-id)
            (delete-by-id type id)))))

;; TODO - Clean this up
(defroutes admin-module-routes
  (GET "/idea/edit*" request (form-layout "Edit Idea Form"
                                          request
                                          (edit-idea-form request
                                                          (:params request))))
  (POST "/idea/edit*" {params :params}
        (save-idea! params (str "edit?id=" (get params "id"))))
  (GET "/idea/delete*" request
       (form-layout "Confirm Delete Idea"
                    request
                    (confirm-delete find-by-id
                                    :idea
                                    properties
                                    (get (:params request) "id"))))
  (POST "/idea/delete*" {params :params}
        (do
          (if (not (form-cancelled? params))
            (delete-by-id :idea (get params "id")))
          (redirect "list")))
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
  (GET "/idea/download*" []
       {:status 200
        :headers {"Content-Type" "application/vnd.ms-excel"
                   "Content-disposition"
                   "attachment;filename=\"ideadb.csv\""}
        :body (download-ideas)})
  (security-edit-user-routes "/admin" (var admin-users-layout) (fn [r] (:uri r))
                             properties user-data-functions)
  (security-login-routes "" main-layout (fn [r] (:uri r))
                         properties user-data-functions))

