;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.dev.forms
  (:use (hiccup core form-helpers)
        (sandbar core stateful-session)
        (sandbar.dev util)))

;;
;; Validation
;; ==========
;;

(defn invalid? [name validation-fn props form-data]
  (if-let [errors (validation-fn props form-data)]
    (do (set-flash-value! name
                          (merge {:form-data form-data} errors))
        true)
    false))

(defn required-field [m k msg]
  (if (empty? (k m)) {k msg}))

;;
;; Forms
;; =====
;;

(defn layout-table
  "Create a table using the layout vector. The layout vector is a vector of
   integers, one for each row indicating the number of cell that go in that
   row. If the number of cells in a row is less than the max value then
   the last cell will be given a colspan value that is large enough to fill
   the table."
  [layout & cell-values]
  (loop [layout (take (count cell-values) (concat layout (repeat 1)))
         max-width (apply max layout)
         cell-values (filter #(not (nil? %)) cell-values)
         rows []]
    (if (seq cell-values)
      (let [row-cell-values (take (first layout) cell-values)
            final-colspan (+ 1 (- max-width (count row-cell-values)))
            row-values (map #(vector 1 %) row-cell-values)
            row-values (if (> final-colspan 1)
                         (concat (butlast row-values)
                                 [[final-colspan (last (last row-values))]])
                         row-values)]
        (recur (rest layout)
               max-width
               (drop (first layout) cell-values)
               (if (seq row-values)
                 (conj rows
                       (vec
                        (concat [:tr]
                                (vec
                                 (map #(let [cell-value (last %)
                                             cell-value (if (vector? cell-value)
                                                          cell-value
                                                          [cell-value])]
                                         (vec
                                          (if (> (first %) 1)
                                            (concat [:td {:valign "top"
                                                          :colspan (first %)}]
                                                    cell-value)
                                            (concat [:td {:valign "top"}]
                                                    cell-value))))
                                      row-values)))))
                 rows)))
      (vec (concat [:table] rows)))))

(defn get-params [key-map params]
  (reduce (fn [a b] (assoc a (keyword b) (get params b)))
          {}
          (map name key-map)))

(defn cancel-button []
  [:input {:type "submit" :value "Cancel" :name "cancel"}])

(defn alt-submit-button [text]
  [:input {:type "submit" :value text :name "submit"}])

(defn create-submit-button [[k v]]
  (cond (= k :submit) (submit-button v)
        :else (alt-submit-button v)))

(defn submit-and-cancel-buttons [horf submit-buttons]
  (let [submit-spec (if (map? submit-buttons)
                      submit-buttons
                      {:submit submit-buttons})]
    (vec
     (concat
      [:span {:class (str horf "-buttons")}]
      (interleave (map create-submit-button submit-spec) (cycle ["&nbsp;"]))
      [(cancel-button)]))))

(defn form-header [form-title submit-buttons]
  [:div {:class "form-header"}
   [:table
    [:tr
     [:td
      [:span {:class "form-title"} form-title]]
     [:td {:align "right"}
      (submit-and-cancel-buttons "header" submit-buttons)]]]])

(defn form-footer [submit-buttons]
  [:div {:class "form-footer"}
   (submit-and-cancel-buttons "footer" submit-buttons)])

(defn standard-form [title action submit-name body]
  [:div {:class "form"}
   (form-to [:post (cpath action)]
           (form-header title submit-name)
           body
           (form-footer submit-name))])

(defn login-form [action submit-name body]
  [:div {:class "form"}
   (form-to [:post (cpath action)]
            [:table {:align "center"}
             [:tr
              [:td
               body]]
             [:tr
              [:td {:align "center"}
               (submit-button submit-name)
             "&nbsp;&nbsp;"
             (reset-button "Reset")]]])])

(defn form-cancelled? [params]
  (= "Cancel" (get params "cancel")))

(defn form-field-label [title req]
  [:div {:class "field-label"} title
   (if (= req :required) [:span {:class "required"} [:big " *"]] "")])

(defn form-textarea
  ([title fname options] (form-textarea title fname options :optional))
  ([title fname options req]
     {fname [(form-field-label title req)
             [:textarea (merge {:name (name fname)} options)]]}))

(defn form-textfield
  "Create a form text field. In each arity, title can be either a string or
   a map of keys to strings. If it is a map then the fname will be looked up
   in this map and the value will be used as the title. In the arity 3 version
   options can either be a map of options or the :required keyword."
  ([title fname] (form-textfield title fname {:size 35} :optional))
  ([title fname options] (if (keyword? options)
                           (form-textfield title fname {:size 35} options)
                           (form-textfield title fname options :optional)))
  ([title fname options req]
     {fname [(form-field-label (if (map? title)
                                 (property-lookup title fname)
                                 title) req)
             [:input
              (merge {:type "Text" :name (name fname) :value ""} options)]]}))

(defn form-password
  "Use form-textfield to create a text field and then change it to a
   password field."
  [& args]
  (let [textfield (first (apply form-textfield args))]
    {(key textfield) [(first (val textfield))
                      [:input (merge (last (last (val textfield)))
                                     {:type "Password"})]]}))

(defn form-checkbox
  "Create a form checkbox. The title can be a map or a string. If it is a map
   then the displayed title will be looked up in the map using fname."
  ([title fname] (form-checkbox title fname {}))
  ([title fname options]
     {fname [[:span {:class "field-label"} (if (map? title)
                                             (property-lookup title fname)
                                             title)] 
             [:input
              (merge {:type "checkbox"
                      :name (name fname)
                      :value "checkbox-true"} options)]]}))

(defn get-yes-no-fields
  "Get Y or N values for all keys in cb-set. These keys represent checkboxes
   which must have either Y or N value. If the checkbox is not present then
   is was not selected and is a N."
  [m params cb-set]
  (let [new-map (reduce
                 (fn [a b]
                   (if (and (contains? cb-set (keyword (key b)))
                            (= "checkbox-true" (val b)))
                     (assoc a (keyword (key b)) "Y")
                     a))
                 m
                 params)]
    (reduce (fn [a b] (if (b a) a (assoc a b "N")))
            new-map
            cb-set)))

(defn wrap-checkboxes-in-group [coll]
  [:div {:class "group"}
     (map #(vector :div {:class "group-checkbox"} %) coll)])

(defn form-multi-checkbox [props fname coll value-fn]
  {fname
   [[:span {:class "group-title"} (property-lookup props fname)]
    (wrap-checkboxes-in-group (map
                               #(let [value (value-fn %)]
                                  [:input
                                   {:type "checkbox" :name fname :value value}
                                   (property-lookup props (keyword value))])
                               coll))]})

(defn get-multi-checkbox
  "Add the key k to the map m where the value of k is is a vector of
   selected values."
  [m params k]
  (let [v (get params (name k))
        value-seq (if (string? v) [v] v)]
    (assoc m k (vec (filter #(not (nil? %)) value-seq)))))

(defn checkbox? [field]
  (let [attrs (second field)]
    (= "checkbox" (:type attrs))))

(defn checkbox-group? [field]
  (let [attrs (second field)]
    (= "group" (:class attrs))))

(defn form-hidden [fname]
  {fname [[:input {:type "hidden" :name (name fname) :value ""}]]})

(defn select-map [coll key-key value-key]
  (apply merge (map #(sorted-map (key-key %) (value-key %)) coll)))

(defn form-select
  ([title fname k v coll opts top]
     (form-select title fname k v coll opts top :optional))
  ([title fname k v coll opts top req]
     (let [s-map (select-map coll k v)]
       {fname [(form-field-label title req)
               (vec
                (concat
                 [:select (merge {:name (name fname)} opts)]
                 [[:option {:value (key (first top))} (val (first top))]] 
                 (map #(vector :option {:value (key %)} (val %))
                      s-map)))]})))

(defmulti set-form-field-value (fn [a b] (first b)))

(defn- set-form-field-value* [form-state input-field update-fn]
  (let [field-name (:name (last input-field))
        previous-val ((keyword field-name) (:form-data form-state))]
    (if previous-val
      (update-fn previous-val)
      input-field)))

(defmethod set-form-field-value :textarea [form-state input-field]
  (set-form-field-value*
   form-state
   input-field
   (fn [previous-value]
     (conj input-field previous-value))))

(defmethod set-form-field-value :select [form-state input-field]
  (let [field-name (:name (second input-field))
        previous-val ((keyword field-name) (:form-data form-state))]
    (if previous-val
      (apply vector
             (map #(if (and (vector? %)
                            (= :option (first %))
                            (= previous-val (:value (second %))))
                     [:option {:value previous-val :selected "selected"}
                      (last %)]
                     %)
                  input-field))
      input-field)))

(defmethod set-form-field-value :input [form-state input-field]
  (set-form-field-value*
   form-state
   input-field
   (fn [previous-value]
     (if (checkbox? input-field)
       (vector :input (if (= previous-value "Y")
                        (assoc (last input-field) :checked "true")
                        (last input-field)))
       (vector :input (assoc (last input-field) :value previous-value))))))

;; Set the value of a multi-checkbox. This will make sure all of the
;; selected checkboxes are checked.
(defmethod set-form-field-value :div [form-state input-field]
  (let [title (second input-field)
        checkboxes (map last (last (last input-field)))
        field-name (:name (second (first checkboxes)))
        field-value (field-name (:form-data form-state))
        value-set (set (if (string? field-value) [field-value] field-value))
        new-checkboxes (map #(vector :input
                                     (let [attrs (second %)]
                                       (if (contains? value-set
                                                     (:value attrs))
                                        (assoc attrs :checked "true")
                                        attrs))
                                     (last %))
                            checkboxes)]
    [:div title (wrap-checkboxes-in-group new-checkboxes)]))

(defn create-form-field-cell
  "Create a cell in a form layout which contains the field label and input.
   This cell will also contain an error message if there is one to display."
  [form-state m]
  (if (= 2 (count (val (first m))))
    (let [entry (first m)
          entry-key (key entry)
          title (first (val entry))
          input-field (last (val entry))
          error-message (entry-key form-state)
          title-row (cond (checkbox? input-field)
                          [:div "&nbsp;"]
                          (checkbox-group? input-field)
                          [:div]
                          :else title)
          field-row (vec
                     (filter #(not (nil? %))
                             [:div
                              (set-form-field-value
                               form-state
                               (if (checkbox-group? input-field)
                                 [:div title input-field]
                                 input-field))
                              (if (checkbox? input-field)
                                title)]))]
      (if error-message
        [:div
         title-row
         [:div {:class "error-message"} error-message]
         field-row]
        [:div title-row field-row]))))

(defn- create-hidden-field [form-state m]
  (let [entry (first m)
        entry-key (key entry)
        input-field (last (val entry))]
    (set-form-field-value form-state input-field)))

(def one-column-layout (repeat 1))

(defn form-layout-grid* [layout form-state coll]
  (let [the-form
        (conj [:div]
              (apply layout-table
                     layout
                     (map #(if-let [cell (create-form-field-cell form-state %)]
                             (vector cell)
                             nil)
                          coll)))]
    (vec (concat the-form
                 (map #(create-hidden-field form-state %)
                      (filter #(= 1 (count (val (first %)))) coll))))))

(defn form-layout-grid
  ([form-name coll request]
     (form-layout-grid one-column-layout form-name coll request {}))
  ([layout form-name coll request]
     (form-layout-grid layout form-name coll request {}))
  ([layout form-name coll request init-data]
     (if-let [form-state (get-flash-value! form-name)]
       (form-layout-grid* layout form-state coll)
       (form-layout-grid* layout {:form-data init-data} coll))))

(defn clean-form-input
  "Set empty values to nil and remove the id if it is nil."
  [m]
  (apply merge
         (map #(hash-map (first %)
                         (let [value (last %)]
                           (if (and (not (keyword? value))
                                    (empty? value))
                             nil
                             value)))
              (if (:id m) m (dissoc m :id)))))

