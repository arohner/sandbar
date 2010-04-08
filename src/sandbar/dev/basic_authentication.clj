;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.dev.basic-authentication
  (:use (compojure core)
        (ring.util [response :only (redirect)])
        (sandbar core auth stateful-session)
        (sandbar.dev forms util)))

;; This namespace should only depend on the fact that there is a
;; username and password. Extract all of the specific user stuff into
;; a protocol.

(defn basic-auth [request]
  (do (session-put! :auth-redirect-uri
                    (:uri request))
      (redirect "/login")))

(defn create-login-from-params
  "Create a map of all login info to verify the identity of this user."
  [load-fn params]
  (let [form-data (get-params [:username :password] params)
        user (first (load-fn :app_user
                             {:username (:username form-data)} {}))
        roles (index-by :id (load-fn :role))]
    (-> form-data
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
   (:uri request)
   "Login"
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

(defn authenticate! [load-fn props params]
  (let [user-data (create-login-from-params load-fn params)
        success (or (session-get :auth-redirect-uri)
                    (property-lookup props :login-page))
        failure "login"]
    (redirect
     (cond (invalid-login?! props user-data) failure
           (not (valid-password? user-data)) failure
           :else (do
                   (session-put! :current-user
                                 {:name (:username user-data)
                                  :roles (:roles user-data)})
                   (session-delete-key! :auth-redirect-uri)
                   success)))))

;;
;; Routes
;; ======
;;

(defn auth-login-routes
  ([layout name-fn props load-fn]
     (auth-login-routes "" layout name-fn props load-fn))
  ([path-prefix layout name-fn props load-fn]
     (routes
      (GET (str path-prefix "/login*") request
           (layout (name-fn request)
                   request
                   (login-page props request)))
      (POST (str path-prefix "/login*") {params :params}
            (authenticate! load-fn props params))
      (GET (str path-prefix "/logout*") []
           (logout! props)))))
