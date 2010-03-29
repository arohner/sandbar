; Copyright (c) Brenton Ashworth. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file COPYING at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns sandbar.basic_authentication
  (:use (compojure core)
        (ring.util [response :only (redirect)])
        (sandbar auth library)))

(defn basic-auth [request]
  (do (put-in-session! request
                       :auth-redirect-uri
                       (:uri request))
      (redirect "/login")))

(defn create-login-from-params
  "Create a map of all login info to verify the identity of this user."
  [load-fn request]
  (println request)
  (let [params (:params request)
        form-data (-> (select-keys params ["username" "password"]))
        user (first (load-fn :app_user
                             {:username (get form-data "username")} {}))
        roles (index-by :id (load-fn :role))]
    (-> {:username (get form-data "username")
         :password (get form-data "password")}
        (assoc :password-hash (:password user))
        (assoc :salt (:salt user))
        (assoc :roles (set
                       (map #(keyword (:name (roles %)))
                            (map :role_id
                                 (load-fn :user_role
                                          {:user_id (:id user)} {}))))))))

(def invalid-login?!
     (partial
      invalid?
      :login
      (fn [props form-data]
        (merge
         (required-field form-data :username
                         (str (property-lookup props :username)
                              " is required."))
         (required-field form-data :password
                         (str (property-lookup props :password)
                              " is required."))))))

(defn login-page [props request]
  (login-form
   (:uri request) "Login"
   (form-layout-grid [1 1]
                     :login
                     [(form-textfield props :username {:size 25} :required)
                      (form-password props :password {:size 25}  :required)]
                     request
                     {})))

(defn valid-password?
  ([user-data] (valid-password? user-data *hash-delay*))
  ([user-data n]
     (= (hash-password (:password user-data) (:salt user-data) n)
        (:password-hash user-data))))

(defn authenticate! [load-fn props request]
  (let [user-data (create-login-from-params load-fn request)
        success (get-from-session request :auth-redirect-uri)
        failure "login"]
    (println user-data)
    (redirect
     (cond (invalid-login?! props user-data request) failure
           (not (valid-password? user-data)) failure
           :else (do
                   (put-user-in-session! request
                                         {:name (:username user-data)
                                          :roles (:roles user-data)})
                   (remove-from-session! request :auth-redirect-uri)
                   success)))))

(defn logout! [props request]
  (let [logout-page (if-let [p (:logout-page props)]
                      (cpath p)
                      (cpath "/"))]
    (redirect
     (do (remove-from-session! request :current-user)
         logout-page))))

;;
;; Routes
;; ======
;;

(defn security-login-routes [path-prefix layout name-fn props data-fns]
  (routes
   (GET (str path-prefix "/login*") request
        (layout (name-fn request)
                request
                (login-page props request)))
   (POST (str path-prefix "/login*") request
         (authenticate! (data-fns :load) props request))
   (GET (str path-prefix "/logout*") request
        (logout! props request))))
