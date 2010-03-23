; Copyright (c) Brenton Ashworth. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file COPYING at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns sandbar.example.ideadb.admin_module
  (:use [compojure.control :only (decorate)]
        [compojure.html]
        [compojure.http [routes :only (defroutes GET POST)]
                        [helpers :only (redirect-to)]
                        [session :only (flash-assoc)]]
        (sandbar library auth userui)
        (sandbar.example.ideadb
         [layouts :only (main-layout form-layout)]
         [model]
         [user_module])))

(defn edit-idea-form [request params]
  (let [form-data (find-by-id :idea (:id params))]
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

(defroutes admin-module-routes
  (GET "/idea/edit*" (form-layout "Edit Idea Form"
                                  request
                                  (edit-idea-form request params)))
  (POST "/idea/edit*"
        (save-idea! request (str "edit?id=" (:id params))))
  (GET "/idea/delete*" (form-layout "Confirm Delete Idea"
                                    request
                                    (confirm-delete find-by-id
                                                    :idea
                                                    properties
                                                    (:id params))))
  (POST "/idea/delete*"
        (do
          (if (not (form-cancelled params))
            (delete-by-id :idea (:id params)))
          (redirect-to "list")))
  (GET "/admin/list*" (main-layout "Administrator"
                                   request
                                   (admin-menu request)))
  (GET "/admin/business-unit*"
       (main-layout "Edit Business Units"
                    request
                    (my-list-editor :business_unit request params)))
  (POST "/admin/business-unit*"
        (my-list-updater :business_unit request params))
  (GET "/admin/category*"
       (main-layout "Edit Categories"
                    request
                    (my-list-editor :idea_category request params)))
  (POST "/admin/category*"
        (my-list-updater :idea_category request params))
  (GET "/admin/status*"
       (main-layout "Edit Status List"
                    request
                    (my-list-editor :idea_status request params)))
  (POST "/admin/status*"
        (my-list-updater :idea_status request params))
  (GET "/admin/type*"
       (main-layout "Edit Types"
                    request
                    (my-list-editor :idea_type request params)))
  (POST "/admin/type*"
        (my-list-updater :idea_type request params))
  (GET "/idea/download*"
       [{:headers {"Content-Type" "application/vnd.ms-excel"
                   "Content-disposition"
                   "attachment;filename=\"ideadb.csv\""}} 
        (download-ideas)])
  (security-edit-user-routes "/admin" main-layout (fn [r] (:uri r))
                             properties user-data-functions)
  (security-login-routes "/idea" main-layout (fn [r] (:uri r))
                         properties user-data-functions))

