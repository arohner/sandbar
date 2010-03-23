; Copyright (c) Brenton Ashworth. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns sandbar.example.ideadb.user_module
  (:use [compojure.html]
        [compojure.http [routes :only (defroutes GET POST)]
                        [helpers :only (redirect-to)]
                        [session :only (flash-assoc)]]
        (sandbar library
                    [security :only (current-username
                                     current-user-roles
                                     any-role-granted?)])
        (sandbar.example.ideadb
         [layouts :only (main-layout list-layout form-layout)]
         [model])))

(set-app-context! "/ideadb")

(def properties
     {:business_unit "Business Unit"
      :idea_category "Category"
      :idea_status "Status"
      :idea_type "Type"
      :idea "Idea"
      :id "ID"
      :name "Name"
      :description "Description"
      :customer_need "Need"
      :originator "Originator"
      :date_entered "Date (yyyy-mm-dd)"
      :category "Category"
      :idea_type "Type"
      :business-unit "Business Unit"
      :status "Status"
      :empty ""
      :username "Username"
      :password "Password"
      :new_password "Password"
      :first_name "First Name"
      :last_name "Last Name"
      :email "Email"
      :account_enabled "Account Enabled"
      :roles "Assigned Roles"
      :admin "Administrator"
      :user "User"
      :logout-page "/idea/new"})

(defn index [request]
  (html 
   [:div "This is the front page of the app. Here is your environment:"
    [:br] [:br]
    [:div "User : " (current-username request)]
    [:div "Role : " (current-user-roles request)]
    [:div "classname : " (request-att request "classname")]
    [:div "subprotocol : " (request-att request "subprotocol")]
    [:div "subname : " (request-att request "subname")]
    [:div "user : " (request-att request "user")]
    [:div "password : " (apply str (map (fn [a] "#")
                                        (request-att request "password")))]
    [:br] [:br]
    (clink-to "/idea/list" "Idea List")]))

;;
;; Idea Management
;; ===============
;;

(defn admin-role? [request]
  (any-role-granted? request :admin))

(defn generate-welcome-message [request]
  (if (not (admin-role? request))
    [:div {:id "welcome-message"}
     (str "Welcome " (current-username request)
          "! The table below displays all of the ideas you have submitted.")]))

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
                                      {:user_id (current-username request)})
                               sort-and-page))))

(defn user-has-ideas? [request]
  (< 0 (count ((idea-table-records-function request) :idea {} {}))))

(defn idea-list [request] 
  (let [admin (admin-role? request)]
    (html
    (generate-welcome-message request)
    (filter-and-sort-table
     request
     {:type :idea :name :idea-table :props properties}
     (if admin 
       (conj idea-table-columns :empty) 
       idea-table-columns)
     (fn [k row-data]
       (cond (= k :name)
             (if admin
               (clink-to (str "/idea/edit?id=" (:id row-data)) (:name row-data))
               (:name row-data))
             (= k :empty)
             (clink-to (str "/idea/delete?id=" (:id row-data)) "Delete")
             :else (k row-data)))
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

(defn create-idea-from-params [request]
  (let [params (:params request)
        idea
        (reduce (fn [a b] (assoc a b (params b)))
                {}
                [:id :description :name :customer_need :originator
                 :category :idea_type :business_unit :status
                 :date_entered :user_id])
        date (if-let [de (:date_entered idea)]
               (if (empty? de) (date-string) de)
               (date-string))
        user (if-let [u (:user_id idea)]
               (if (empty? u) (current-username request) u)
               (current-username request))
        idea (-> idea
                 (assoc :date_entered date)
                 (assoc :user_id user)
                 (assoc :type :idea))]
    (clean-form-input idea)))

(def invalid-idea?!
     (partial
      invalid?
      :idea
      (fn [props form-data]
        (merge
         (required-field form-data :description "Please enter a description.")
         (required-field form-data :name "Please enter a name.")
         (required-field form-data :customer_need
                         "Please enter a customer need.")))
      properties))

(defn save-idea! [request action]
  (let [form-data (create-idea-from-params request)
        submit (-> request :params :submit)
        success (if (= submit "Save and New")
                  (cpath "/idea/new")
                  (cpath "/idea/list"))
        failure (cpath (str "/idea/" action))]
    (redirect-to
     (cond (form-cancelled (:params request)) success
           (invalid-idea?! form-data request) failure
           :else (do
                   (set-flash-value! :user-message
                                     (if (= action "new")
                                       "Your idea has been successfully
                                        submitted."
                                       "The idea has been updated.")
                                     request)
                   (save form-data)
                   success)))))

;;
;; Routes
;; ======
;;

(defn with-db-configured [handler]
  (fn [request]
    (configure-database request)
    (handler request)))

(defroutes user-module-routes
  (GET "/" (redirect-to (cpath "/idea/list")))
  (GET "/idea/status" (main-layout "Home" request (index request)))
  (GET "/idea/new*" (form-layout "New Idea Form"
                                  request
                                  (new-idea-form request)))
  (POST "/idea/new*" (save-idea! request "new"))
  (GET "/idea/list*" (if (or (admin-role? request)
                             (user-has-ideas? request))
                       (list-layout "Idea List"
                                    request
                                    (idea-list request))
                       (redirect-to (cpath "/idea/new"))))
  (GET "/idea/permission-denied" (main-layout "Permission Denied"
                                         request
                                         (permission-denied-page))))

