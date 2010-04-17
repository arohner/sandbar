;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.example.basic-auth-demo
  "Simple example of using sandbar.auth and sandbar.dev.basic-authentication
   to do form based authentication."
  (:use (ring.adapter jetty)
        (ring.middleware params file file-info)
        (compojure core)
        (hiccup core page-helpers)
        (sandbar core stateful-session auth)
        (sandbar.dev html basic-authentication validation)
        (sandbar.example [auth-demo :only (load-data-from
                                           layout
                                           home-view admin-view member-view
                                           permission-denied-view)])))

;; Building on the auth-demo code, adding form based
;; authentication. Please check out that demo first in order to
;; understand the basics of authentication. We are re-using the model
;; and view code from that example here. Notes are included.

;;
;; Properties
;; ==========
;; A map is used to configure the text that is displayed on the
;; login form as well as configuring the error messages that are
;; displayed. This is optional, there are sane defaults.

(def properties
     {:username "Username (admin or member)"
      :password "Password (same as above)"
      :username-validation-error "Enter either admin or member"
      :password-validation-error "Enter a password!"})

;;
;; Implement BasicAuthUser
;; =======================
;; Implement the BasicAuthUser protocol to load your user data and
;; validate the users password.

(deftype UserModel [] BasicAuthUser
  (load-login-user
   [this username password]
   (let [login {:username username :password password}]
     (cond (= username "member")
          (merge login {:roles #{:member}})
          (= username "admin")
          (merge login {:roles #{:admin}})
          :else login)))
  (validate-password
   [this m]
   (if (= (:password m) (:username m))
     m
     (add-validation-error m "Incorrect username or password!"))))

;;
;; Routes
;; ======
;; We add basic-auth-routes to get the login form, passing it our
;; layout, properties and the UserModel. We also create a route
;; for the permission-denied page which is where the user will be
;; redirected when they attempt to visit a page that they do not have
;; permission to view.

(defroutes my-routes
  (GET "/home*" [] (layout (home-view)))
  (GET "/member*" [] (layout (member-view)))
  (GET "/admin*" [] (layout (admin-view)))
  (GET "/permission-denied*" [] (layout (permission-denied-view)))
  (basic-auth-routes (fn [r c] (layout c)) properties (UserModel))
  (ANY "*" [] (layout (home-view))))

;;
;; Configure Channel Security
;; ==========================
;; This will ensure that the login page is encrypted and all other
;; pages are not. We add a special config for resources so that we
;; don't get mixed content on secure pages.

(def security-config
     [#"/login.*"                   :ssl 
      #".*.css|.*.js|.*.png|.*.gif" :any-channel 
      #".*"                         :nossl])

;;
;; Add the with-security middleware to enable authorization and
;; authentication passing it the basic-auth authorization function. We
;; also add middleware for channel security.

(def app
     (-> my-routes
         (with-security basic-auth)
         wrap-stateful-session
         (wrap-file "public")
         wrap-file-info
         (with-secure-channel security-config 8080 8443)))

(defn run []
  (run-jetty (var app) {:join? false :ssl? true :port 8080 :ssl-port 8443
                        :keystore "my.keystore"
                        :key-password "foobar"})) 
