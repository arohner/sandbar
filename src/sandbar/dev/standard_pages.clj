;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.dev.standard-pages
  "Pages that are not specific to a single application and may be refered to
   within the sandbar library."
  (:use (hiccup core)
        (sandbar.dev forms)))

(defn confirm-delete
  ([find-by-id-fn type props id]
     (confirm-delete find-by-id-fn type :name props id))
  ([find-by-id-fn type name-fn props id]
     (let [list-item (find-by-id-fn type id)]
       (standard-form
        (str "Delete " (props type))
        "delete"
        "Yes - Delete it"
        [:div (if list-item
                [:input {:type "Hidden" :name "id" :value id}])
         [:div (str "Are you sure you want to delete the "
                    (props type)
                    " named "
                    (name-fn list-item)
                    "?")]]))))

(defn page-not-found-404 []
  [:div
   [:h2 "Page Not Found (404)"]
   [:div "The page that you have requested was not found on this server.
         Please check the URL and try again."]])

(defn permission-denied-page []
  [:div
   [:h2 "Permission Denied"]
   [:div "You are trying to access a page for which you do not have the
          correct permissions."]
   [:br]])

(defn authentication-error-page []
  [:div
   [:h2 "Authentication Error"]
   [:div "An error has occured while trying to authenticate a user. This can
          happen when a loop is encountered. Some part of the system is
          raising an authentication error but the user has been authenticated."]
   [:br]])
