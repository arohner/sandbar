;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns example.auth-demo
  "Simple example of using sandbar.auth with authorization at the function
   level."
  (:use (ring.adapter jetty)
        (ring.middleware params file)
        (compojure core)
        (hiccup core page-helpers)
        (sandbar core stateful-session auth)))

;; Demonstrating how to use sandbar.auth. We create a web page with
;; two links, one to a member page and one to an admin
;; page. The home page is visible to the public, the member page is
;; visible to members and the admin page is visible to admins. The
;; first link that you click on will authenticate you with that
;; role. You can then try to go to the other link and see that
;; permission is denied. My notes are included.

;;
;; Model
;; ====
;; Protect data at the model. Use ensure-any-role-if to ensure that
;; only admins can see admin data and members can see member data.

(defn load-data-from [table]
  (ensure-any-role-if (= table :admin) #{:admin}
                      (= table :member) #{:member}
                      (str "Here be " (name table) " data.")))

;;
;; View
;; ====
;; Note the use of any-role-granted? to selectively show text based on
;; the user's role. Also note the use of current-username to display
;; the current user's username if a user is logged in. We also include
;; some basic styling.

(defn layout [content]
  (html
   (doctype :html4)
   [:html
    [:head
     (stylesheet "sandbar.css")]
    [:body
     [:h2 "Auth Demo"]
     content
     [:br]
     [:div
      (cond (any-role-granted? :admin)
            "If you can see this then you are an admin!"
            (any-role-granted? :member)
            "If you can see this then you are a member!"
            :else "Click on one of the links above to log in.")]  
     [:div (if-let [username (current-username)]
             [:div (str "You are logged in as " username ". ")
              (link-to "logout" "Logout")]
             "You are current not logged in.")]]]))

;;
;; Notice that there is no authorization stuff in the view. We simply
;; load the data that we need. The data is protected in the model.

(defn home-view []
  [:div
   [:h3 "Home"]
   [:p (load-data-from :public)]
   [:div (link-to "member" "Member Page")]
   [:div (link-to "admin" "Admin Page")]])

(defn member-view []
  [:div
   [:h3 "Member Data"]
   [:p (load-data-from :member)]
   [:div (link-to "home" "Home")]])

(defn admin-view []
  [:div
   [:h3 "Admin"]
   [:p (load-data-from :admin)]
   [:div (link-to "home" "Home")]])

(defn permission-denied-view []
  [:div
   [:h3 "Permission Denied"]
   [:div (link-to "home" "Home")]])

;;
;; Routes
;; ======
;; Two special routes are created. One to logout, which calls the
;; logout! function in sandbar.auth, and one for the permission-denied
;; page which is where the user will be redirected when they attempt
;; to visit a page that they do not have permission to view.

(defroutes my-routes
  (GET "/home*" [] (layout (home-view)))
  (GET "/member*" [] (layout (member-view)))
  (GET "/admin*" [] (layout (admin-view)))
  (GET "/logout*" [] (logout! {}))
  (GET "/permission-denied*" [] (layout (permission-denied-view))) 
  (ANY "*" [] (layout (home-view))))

;;
;; Authorization Function
;; ======================
;; This is our authorization function. The function must be a function
;; of the request that returns either a redirect or an authorized
;; user. For simplicity, we are authorizing based on the first URI
;; that is visited. Note that a valid user is a map with the keys
;; :name and :roles. This function could do anything so it is easy to
;; implement your own authorization scheme.

(defn authorize [request]
  (let [uri (:uri request)]
    (cond (= uri "/member") {:name "member" :roles #{:member}}
          (= uri "/admin") {:name "admin" :roles #{:admin}})))

;;
;; Add the with-security middleware to enable authorization and
;; authentication passing it your authorization function.

(def app
     (-> my-routes
         (with-security authorize)
         wrap-stateful-session
         (wrap-file "public")))

(defn run []
  (run-jetty (var app) {:join? false :port 8080}))
