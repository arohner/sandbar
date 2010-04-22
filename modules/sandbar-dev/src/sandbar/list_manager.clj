;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.list-manager
  "List management."
  (:use (ring.util [response :only (redirect)])
        (compojure core) 
        (hiccup core page-helpers)
        (sandbar stateful-session forms tables standard-pages validation))
  (:require [clojure.contrib.str-utils2 :as string]))

;;
;; Simple Lists
;; ============
;;

(defn list-editor-display-list [adapter finished]
  (let [paged-list ((:paged-list adapter) {})
        list-name (:visible-name adapter)]
    [:div
     [:div (link-to finished "Finished")]
     [:h2 (str list-name " List")]
     [:div (link-to "add" (str "Add " list-name))]
     (standard-table
      (:properties adapter)
      [:id :name :empty]
      (fn [k row-data]
        (cond (= k :empty) [:div (link-to (str "edit?id=" (:id row-data))
                                          "Edit") "&nbsp;"
               (link-to (str "delete?id=" (:id row-data)) "Delete")]
              :else (row-data k)))
      paged-list)]))

(defn list-editor-form
  ([adapter request]
     (list-editor-form adapter request nil))
  ([adapter request id]
     (let [list-item (if id ((:find-by-id adapter) id) nil)]
       (standard-form
        (str (if list-item "Edit " "Add ") (:visible-name adapter))
        (if list-item "edit" "add")
        "Save"
        [:div (if list-item
                [:input {:type "Hidden" :name "id" :value id}])
         (form-layout-grid
          (:id adapter)
          [(form-textfield "Name:"
                           :name
                           {:value (if list-item (:name list-item) "")
                            :size 65} :required)]
          request)]))))

(defn get-action [request]
  (get (:route-params request) "*"))

(defn list-editor [adapter finished request]
  (let [params (:params request)
        action (get-action request)]
    (cond (.startsWith action "/list")
          (list-editor-display-list adapter finished)
          (.startsWith action "/add")
          (list-editor-form adapter request)
          (.startsWith action "/edit")
          (list-editor-form adapter request (get params "id"))
          (.startsWith action "/delete")
          (confirm-delete (fn [t id] ((:find-by-id adapter) id))
                          (:id adapter)
                          (:properties adapter)
                          (get params "id"))
          :else "This action is not implemented...")))

(defn save-list-item [adapter form-data action]
  (if-valid #(non-empty-string % :name "Please enter a name.") form-data
            #(do ((:save adapter) %)
                 "list")
            (store-errors-and-redirect (:id adapter) action)))

(defn list-updater [adapter request]
  (let [params (:params request)
        action (get-action request)]
    (redirect
     (cond (form-cancelled? params) "list"
           (.startsWith action "/add")
           (save-list-item adapter
                           {:type (:id adapter)
                            :name (get params "name")}
                           "add")
           (.startsWith action "/edit")
           (save-list-item adapter
                           {:type (:id adapter)
                            :name (get params "name")
                            :id (get params "id")}
                           "edit")
           (.startsWith action "/delete")
           (do
             ((:delete-by-id adapter) (get params "id"))
             "list")
           :else "list"))))

;;
;; Routes
;; ======
;;

(defn urify-keyword [k]
  (string/replace (name k) #"_" "-"))

(defn list-manager-routes [layout prefix finished adapter]
  (routes
   (GET (str prefix "/" (urify-keyword (:id adapter)) "*") request
        (layout request
                (list-editor adapter finished request)))
   (POST (str prefix "/" (urify-keyword (:id adapter)) "*")
         request
         (list-updater adapter request))))
