;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.dev.list-manager
  "Editor for simple lists."
  (:use (ring.util [response :only (redirect)])
        (compojure core) 
        (hiccup core page-helpers)
        (sandbar stateful-session)
        (sandbar.dev forms tables standard-pages validation))
  (:require [clojure.contrib.str-utils2 :as string]))

(defprotocol ListAdapter
  "Functions required to manage a simple list of items."
  (paged-list [this filters] "Return a paged list of items.")
  (find-by-id [this id] "Find a single item by id.")
  (save-map [this m] "Save a single item.")
  (delete-by-id [this id] "Delete the item with this id.")
  (get-visible-name [this] "Get the name to display for this list.")
  (get-identifier [this] "Get the keyword identifier for this list.")
  (get-properties [this] "Get the properties for this list."))

;;
;; Simple Lists
;; ============
;;

(defn list-editor-display-list [adapter finished]
  (let [paged-list (paged-list adapter {})
        list-name (get-visible-name adapter)]
    [:div
     [:div (link-to finished "Finished")]
     [:h2 (str list-name " List")]
     [:div (link-to "add" (str "Add " list-name))]
     (standard-table
      (get-properties adapter)
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
     (let [list-item (if id (find-by-id adapter id) nil)]
       (standard-form
        (str (if list-item "Edit " "Add ") (get-visible-name adapter))
        (if list-item "edit" "add")
        "Save"
        [:div (if list-item
                [:input {:type "Hidden" :name "id" :value id}])
         (form-layout-grid
          (get-identifier adapter)
          [(form-textfield "Name:"
                           :name
                           {:value (if list-item (:name list-item) "")
                            :size 65} :required)]
          request)]))))

(defn list-editor [adapter finished request]
  (let [params (:params request)
        action (params :*)]
    (cond (.startsWith action "/list")
          (list-editor-display-list adapter finished)
          (.startsWith action "/add")
          (list-editor-form adapter request)
          (.startsWith action "/edit")
          (list-editor-form adapter request (get params "id"))
          (.startsWith action "/delete")
          (confirm-delete (fn [t id] (find-by-id adapter id))
                          (get-identifier adapter)
                          (get-properties adapter)
                          (get params "id"))
          :else "This action is not implemented...")))

(defn save-list-item [adapter form-data action]
  (if-valid #(non-empty-string % :name "Please enter a name.") form-data
            #(do (save-map adapter %)
                 "list")
            (store-errors-and-redirect (get-identifier adapter) action)))

(defn list-updater [adapter params]
  (let [action (params :*)]
    (redirect
     (cond (form-cancelled? params) "list"
           (.startsWith action "/add")
           (save-list-item adapter
                           {:type (get-identifier adapter)
                            :name (get params "name")}
                           "add")
           (.startsWith action "/edit")
           (save-list-item adapter
                           {:type (get-identifier adapter)
                            :name (get params "name")
                            :id (get params "id")}
                           "edit")
           (.startsWith action "/delete")
           (do
             (delete-by-id adapter (get params "id"))
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
   (GET (str prefix "/" (urify-keyword (get-identifier adapter)) "*") request
        (layout request
                (list-editor adapter finished request)))
   (POST (str prefix "/" (urify-keyword (get-identifier adapter)) "*")
         {params :params}
         (list-updater adapter params))))
