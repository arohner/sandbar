;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.basic-authentication
  "Provide support for form based authentication."
  (:use (compojure core)
        (ring.util [response :only (redirect)])
        (sandbar [auth :only (logout!)]
                 [stateful-session :only (session-get
                                          session-put!
                                          set-flash-value!
                                          session-delete-key!)]
                 [core :only (property-lookup)]
                 [forms :only (form-layout-grid
                               form-textfield
                               form-password
                               login-form
                               get-params
                               store-errors-and-redirect)]
                 [validation :only (build-validator
                                    non-empty-string
                                    if-valid)])))

(defn basic-auth [request]
  (do (session-put! :auth-redirect-uri
                    (:uri request))
      (redirect "/login")))

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

(defn login-validator [user-model props]
  (let [pw-validator #((:validate-password user-model) %)]
    (build-validator (non-empty-string :username :password props)
                     :ensure
                     pw-validator)))

(defn authenticate! [user-model props params]
  (let [input (get-params [:username :password] params)
        user-data ((:load-login-user user-model)
                                   (:username input)
                                   (:password input))
        success (or (session-get :auth-redirect-uri)
                    (property-lookup props :login-page))
        failure "login"]
    (redirect
     (if-valid (login-validator user-model props) user-data
               #(do
                  (session-put! :current-user
                                {:name (:username %)
                                 :roles (:roles %)})
                  (session-delete-key! :auth-redirect-uri)
                  success)
               #(do (set-flash-value!
                     :login
                     (merge {:form-data (dissoc %1 :username :password)} %2))
                    failure)))))

(defn basic-auth-routes
  ([layout props user-model]
     (basic-auth-routes "" layout props user-model))
  ([path-prefix layout props user-model]
     (routes
      (GET (str path-prefix "/login*") request
           (layout request
                   (login-page props request)))
      (POST (str path-prefix "/login*") {params :params}
            (authenticate! user-model props params))
      (GET (str path-prefix "/logout*") []
           (logout! props)))))
