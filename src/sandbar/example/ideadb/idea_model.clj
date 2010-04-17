;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.example.ideadb.idea-model
  (:use (ring.util [response :only (redirect)])
        (sandbar core
                 [auth :only (current-username
                              current-user-roles
                              any-role-granted?)]
                 stateful-session)
        (sandbar.dev tables forms html util validation)
        (sandbar.example.ideadb properties model)))

;; This need to be cleaned up. The model should not even know it is
;; running in a web application. Move any web specific code to the
;; view or controller.

(def idea-table-columns
     [{:column :id :actions #{:sort}}
      :name
      :description
      :customer_need
      {:column :originator :actions #{:sort :filter}} 
      {:column :date_entered :actions #{:sort :filter} :align :center} 
      {:column :category :actions #{:sort :filter}} 
      {:column :idea_type :actions #{:sort :filter}} 
      {:column :business_unit :actions #{:sort :filter}} 
      {:column :status :actions #{:sort :filter}}])

(defn idea-table-records-function [request]
  (if (admin-role? request)
    filter-and-sort-records
    (fn [type filters sort-and-page]
      (filter-and-sort-records type
                               (merge filters
                                      {:user_id (current-username)})
                               sort-and-page))))

(defn user-has-ideas? [request]
  (< 0 (count ((idea-table-records-function request) :idea {} {}))))

(defn create-idea-from-params [params]
  (let [idea
        (get-params [:id :description :name :customer_need :originator
                     :category :idea_type :business_unit :status
                     :date_entered :user_id]
                    params)
        date (if-let [de (:date_entered idea)]
               (if (empty? de) (date-string) de)
               (date-string))
        user (if-let [u (:user_id idea)]
               (if (empty? u) (current-username) u)
               (current-username))
        idea (-> idea
                 (assoc :date_entered date)
                 (assoc :user_id user)
                 (assoc :type :idea))]
    (clean-form-input idea)))

(def idea-validator
     (build-validator (non-empty-string :description
                                        "Please enter a description.")
                      (non-empty-string :name
                                        "Please enter a name.")
                      (non-empty-string :customer_need
                                        "Please enter a customer need.")))
(defn validation-success-fn [action success]
  (fn [form-data]
    (do
      (save form-data)
      (set-flash-value! :user-message (if (= action "new")
                                        "Your idea has been successfully
                                         submitted."
                                        "The idea has been updated."))
      success)))

(defn save-idea! [params action]
  (redirect
   (let [submit (get params "submit")
         success (if (= submit "Save and New")
                   (cpath "/idea/new")
                   (cpath "/idea/list"))]
     (if (form-cancelled? params)
       success
       (let [form-data (create-idea-from-params params)
             failure (cpath (str "/idea/" action))]
         (if-valid idea-validator form-data
                   (validation-success-fn action success)
                   (store-errors-and-redirect :idea failure)))))))
