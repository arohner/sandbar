;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.example.ideadb.idea-view
  (:use (hiccup core)
        (sandbar core
                 [auth :only (current-username
                              current-user-roles
                              any-role-granted?)]
                 stateful-session)
        (sandbar.dev tables forms html util validation)
        (sandbar.example.ideadb properties model idea-model)))

(defn generate-welcome-message [request]
  (if (not (admin-role? request))
    [:div {:id "welcome-message"}
     (str "Welcome " (current-username)
          "! The table below displays all of the ideas you have submitted.")]))

(defn index [request]
  (let [admin (admin-role? request)]
    (generate-welcome-message request)
    (html
     (filter-and-sort-table
      (:params request)
      {:type :idea :name :idea-table :props properties}
      (if admin 
        (conj idea-table-columns :empty)
        idea-table-columns)
      (fn [k row-data]
        (cond (= k :name)
              (if admin
                (clink-to (str "/idea/edit?id=" (:id row-data))
                          (:name row-data))
                (:name row-data))
              (= k :empty)
              (clink-to (str "/idea/delete?id=" (:id row-data)) "Delete")
              :else (or (k row-data) "")))
      (idea-table-records-function request)))))

(defn public-idea-fields []
  [(form-textarea "What is your idea?  Describe your new idea in 100 words
                   or less?"
                  :description {:cols 75 :rows 10} :required)
   (form-textfield "Do you have a name for your idea?"
                   :name {:size 70} :required)
   (form-textarea "What consumer need would be met by this new idea?"
                  :customer_need {:cols 75 :rows 5} :required)
   (form-textfield "Who can we thank for this idea? (Optional)"
                   :originator {:size 70})])

(defn admin-idea-fields []
  [(form-select "Category"
               :category :name :name
               (get-list :idea_category)
               {}
               {"" "Select a Category..."})
   (form-select "Type"
                :idea_type :name :name
                (get-list :idea_type)
                {}
                {"" "Select a Type..."})
   (form-select "Business Unit"
                :business_unit :name :name
                (get-list :business_unit)
                {}
                {"" "Select a Business Unit..."})
   (form-select "Status"
                :status :name :name
                (get-list :idea_status)
                {}
                {"" "Select a Status..."})])

(defn new-idea-form [request]
  (let [admin (admin-role? request)]
    (standard-form "Submit an Idea" "/idea/new" 
                   (if admin
                     {:submit "Save and Close" :submit-and-new "Save and New"}
                     "Submit My Idea") 
                   (form-layout-grid [1 1 1 1 4]
                                     :idea
                                     (if admin
                                       (concat
                                        (public-idea-fields)
                                        (admin-idea-fields))
                                       (public-idea-fields))
                                     request))))

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

;; This should be able at the same URI as the list. It is the
;; same data with a different format.
(defn download-ideas []
  (let [data (filter-and-sort-records :idea {} {})
        fields [:id :name :description :customer_need :originator
                :date_entered :category :idea_type :business_unit :status]
        data (concat [(map #(% properties) fields)]
                     (map #(map (fn [field] (% field)) fields) data))]
    (format-csv data)))
