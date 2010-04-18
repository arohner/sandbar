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

(def idea-validator
     (build-validator (non-empty-string :description
                                        "Please enter a description.")
                      (non-empty-string :name
                                        "Please enter a name.")
                      (non-empty-string :customer_need
                                        "Please enter a customer need.")))

(defn save-idea
  "Given an idea or parameters from which an idea can be created, save the
   idea. The single argument version will return nil if the idea is valid
   and saved and return errors if it does not pass validation. You may also
   pass functions to be run on success and failure."
  ([idea] (save-idea (fn [i] nil) (fn [i e] e)))
  ([idea on-success on-failure]
     (if-valid idea-validator
               (if (contains? idea :type)
                 idea
                 (create-idea-from-params idea))
               (fn [i]
                 (do
                   (save i)
                   (on-success i)))
               on-failure)))

