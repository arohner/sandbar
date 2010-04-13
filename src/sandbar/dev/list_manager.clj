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
        (hiccup core page-helpers)
        (sandbar stateful-session)
        (sandbar.dev forms tables standard-pages validation)))

(defn list-editor-display-list [paged-list-fn type props finished]
  (let [paged-list (paged-list-fn type {})]
    [:div
     [:div (link-to finished "Finished")]
     [:h2 (str (props type) " List")]
     [:div (link-to "add" (str "Add " (props type)))]
     (standard-table
      props
      [:id :name :empty]
      (fn [k row-data]
        (cond (= k :empty) [:div (link-to (str "edit?id=" (:id row-data))
                                          "Edit") "&nbsp;"
               (link-to (str "delete?id=" (:id row-data)) "Delete")]
              :else (row-data k)))
      paged-list)]))

(defn list-editor-form
  ([find-by-id-fn type props request]
     (list-editor-form find-by-id-fn type props request nil))
  ([find-by-id-fn type props request id]
     (let [list-item (if id (find-by-id-fn type id) nil)]
       (standard-form
        (str (if list-item "Edit " "Add ") (props type))
        (if list-item "edit" "add")
        "Save"
        [:div (if list-item
                [:input {:type "Hidden" :name "id" :value id}])
         (form-layout-grid
          type
          [(form-textfield "Name:"
                           :name
                           {:value (if list-item (:name list-item) "")
                            :size 65} :required)]
          request)]))))


(defn list-editor [paged-list-fn find-by-id-fn props finished
                   type request params]
  (let [action (params :*)]
    (cond (.startsWith action "/list")
          (list-editor-display-list paged-list-fn type props finished)
          (.startsWith action "/add")
          (list-editor-form find-by-id-fn type props request)
          (.startsWith action "/edit")
          (list-editor-form find-by-id-fn type props request (get params "id"))
          (.startsWith action "/delete")
          (confirm-delete find-by-id-fn type props (get params "id"))
          :else "This action is not implemented...")))

(defn save-list-item [save-fn type form-data action]
  (if-valid #(non-empty-string % :name "Please enter a name.") form-data
            #(do (save-fn %)
                 "list")
            (store-errors-and-redirect type action)))

(defn list-updater [save-fn delete-by-id-fn type params]
  (let [action (params :*)]
    (redirect
     (cond (form-cancelled? params) "list"
           (.startsWith action "/add")
           (save-list-item save-fn
                           type
                           {:type type :name (get params "name")}
                           "add")
           (.startsWith action "/edit")
           (save-list-item save-fn
                           type
                           {:type type
                            :name (get params "name")
                            :id (get params "id")}
                           "edit")
           (.startsWith action "/delete")
           (do
             (delete-by-id-fn type (get params "id"))
             "list")
           :else "list"))))
