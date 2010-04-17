;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.example.ideadb.control.idea
  (:use (hiccup core)
        (ring.util [response :only (redirect)])
        (sandbar core
                 [auth :only (current-username
                              current-user-roles
                              any-role-granted?)]
                 stateful-session)
        (sandbar.dev tables forms html util validation standard-pages)
        (sandbar.example.ideadb properties
                                model
                                layouts
                                idea-model))
  (:require (sandbar.example.ideadb [idea-view :as view])))

(defn idea-list [request]
  (if (or (admin-role? request)
          (user-has-ideas? request))
    (list-layout "Idea List"
                 request
                 (view/index request))
    (redirect (cpath "/idea/new"))))

;; This should be included in the above function. It is the same view
;; but a differnet content type.
(defn idea-download [request]
  {:status 200
   :headers {"Content-Type" "application/vnd.ms-excel"
             "Content-disposition"
             "attachment;filename=\"ideadb.csv\""}
   :body (view/download-ideas)})

(defn new-idea [request]
  (form-layout "New Idea Form"
               request
               (view/new-idea-form request)))

(defn new-idea-post [{params :params}]
  (save-idea! params "new"))

(defn edit-idea [request]
  (form-layout "Edit Idea Form"
               request
               (view/edit-idea-form request
                               (:params request))))

(defn edit-idea-post [{params :params}]
  (save-idea! params (str "edit?id=" (get params "id"))))

(defn delete-idea [request]
  (form-layout "Confirm Delete Idea"
               request
               (confirm-delete find-by-id
                               :idea
                               properties
                               (get (:params request) "id"))))

(defn delete-idea-post [{params :params}]
  (do
    (if (not (form-cancelled? params))
      (delete-by-id :idea (get params "id")))
    (redirect "list")))
