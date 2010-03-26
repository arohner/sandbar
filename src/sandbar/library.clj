; Copyright (c) Brenton Ashworth. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file COPYING at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns sandbar.library
  (:use [compojure.html]
        [compojure.http.helpers :only (redirect-to)]
        [clojure.contrib.str-utils :only (re-split re-partition)]))


;;
;; Util
;; ====
;;

(defn random-between [lo hi]
  (let [r (java.util.Random.)
        lo (if (char? lo) (int lo) lo)
        hi (if (char? hi) (int hi) hi)
        n (+ 1 (- hi lo))]
    (+ lo (Math/abs (mod (. r nextInt) n)))))

(defn random-string [lo hi]
  (loop [length (random-between lo hi)
         v []]
    (if (> length 0)
      (let [j (random-between 1 3)]
        (recur (dec length)
               (conj v
                     (cond (= j 1) (char (random-between \a \z))
                           (= j 2) (char (random-between \A \Z))
                           (= j 3) (char (random-between \1 \9))))))
      (apply str v))))

(defn index-by [k coll]
  (reduce (fn [a b]
            (let [v (k b)]
              (assoc a v b)))
          {}
          coll))

(defn path-to-seq [path]
  (filter #(not (.equals % "")) (re-split #"/" path)))

(defn to-keywords [coll]
  (map #(keyword %) coll))

(defn seq-to-path [coll]
  (apply str (interleave (repeat "/") coll)))

(defn without-ext [s]
  (apply str (interpose "." (reverse (rest (reverse (re-split #"[.]" s)))))))

(defn date-string
  ([] (date-string (java.util.Date.)))
  ([d]
     (let [formatter (java.text.SimpleDateFormat. "yyyy-MM-dd")]
       (.format formatter d))))

(defn format-csv [coll-of-rows]
  (let [data (map #(interpose "," %) coll-of-rows)]
    (apply str
           (map #(if (nil? %) "\"\""  (if (and (not (= % "\n"))
                                               (not (= % ",")))
                                        (str "\"" % "\"")
                                        %))
                           (reduce (fn [a b] (concat a b ["\n"])) [] data)))))

(defn property-lookup [p k]
  (k p (name k)))


;;
;; State
;; =====
;;

(def app-context (atom ""))

(defn set-app-context! [context]
  (swap! app-context (fn [a b] b) context))

;;
;; HTML
;; ====
;;

; Standard project layout
(def css-path "/public/css/")
(def image-path "/public/images/")
(def js-path "/public/js/")

(defn cpath [path]
  (if (.startsWith path "/")
    (str @app-context path)
    path))

(defn remove-cpath [path]
  (let [c @app-context]
    (if (not (empty? c))
      (apply str (drop (count c) path))
      path)))

(defn clink-to [path title]
  (link-to (cpath path) title))

(defn stylesheet [name]
  (include-css (cpath (str css-path name))))

(defn javascript [name]
  (include-js (cpath (str js-path name))))

(defn icon [name]
  [:link {:rel "icon" :type "image/png" :href (cpath (str image-path name))}])

(defn image
  ([name] (image name {:alt name}))
  ([name attrs] 
    [:img (merge {:src (cpath (str image-path name)) :border "0"} attrs)]) 
  ([name mouseover attrs]
     (let [c-path (cpath image-path)]
       [:img (merge {:src (str c-path name) :border "0" 
           :onmouseout (str "this.src='" c-path name "'") 
           :onmouseover (str "this.src='" c-path mouseover "'")} attrs)])))

(defn image-link
  ([path name] (image-link path name {:alt name})) 
  ([path name attrs]
    (link-to (str (cpath path)) (image name attrs)))
  ([path name mouseover attrs]
     (link-to (str (cpath path)) (image name mouseover attrs))))

(defn redirect-302 [page]
  {:status 302
   :headers {"Location" (cpath page)}
   :body ""})

(defn redirect-301 [url]
  {:status 301
   :headers {"Location" (cpath url)}})

(defn redirect? [m]
  (or (= (:status m) 302)
      (= (:status m) 301)))

(defn append-to-redirect-loc
  "Append the uri-prefix to the value of Location in the headers of the
   redirect map."
  [m uri-prefix]
  (if (or (nil? uri-prefix) (empty? uri-prefix))
    m
    (let [loc (remove-cpath ((:headers m) "Location"))]
      (if (re-matches #".*://.*" loc)
        m
        (merge m {:headers {"Location" (cpath (str uri-prefix loc))}})))))


;;
;; Flash & Session
;; ===============
;;
;; This is currently server side state. In the future you can use your
;; in memory database to keep this information. If you need to scale
;; then you can move that database to another server.
;;

(def application-flash (atom {}))

(defn set-flash-value!
  ([id v]
     (swap! application-flash (fn [a b] (merge a {id b})) v))
  ([k v request]
     (let [id (str (-> request :session :id) "-" k)]
       (set-flash-value! id v))))

(defn get-flash-value! [key request]
  (let [id (str (-> request :session :id) "-" key)
        value (@application-flash id)]
    (if value
      (do
        (swap! application-flash (fn [a b] (dissoc a b)) id)
        value))))

;; Change this to be the session
(def session (atom {}))

;; Change this to clean up the session
(defn clean-up-session!
  "Remove table states that have not been used within the last 12 hours."
  []
  (let [now (.getTime (java.util.Date.))
        cutoff (- now (* 1000 60 60 12))]
    (swap! session
           (fn [a b]
             (let [remove-keys
                   (map first
                        (filter #(< (last %) b)
                                (map #(vector (key %) (:last-access (val %)))
                                     @session)))]
               (apply dissoc a remove-keys)))
           cutoff)))

(defn update-session! [request update-fn value]
  (let [id (keyword (-> request :session :id))]
    (swap! session
          (fn [a b]
            (let [result (update-fn a b)]
              (assoc-in result [id :last-access]
                        (.getTime (java.util.Date.)))))
          value))
  (future (clean-up-session!))
  @session)

(defn put-in-session! [request k v]
  (let [id (keyword (-> request :session :id))]
    (update-session! request (fn [a b] (assoc-in a [id k] b)) v)))

(defn get-from-session [request k]
  (let [id (if-let [id (-> request :session :id)]
             (keyword id)
             nil)]
    (if id
      (-> @session id k)
      nil)))

(defn remove-from-session! [request k]
  (let [id (keyword (-> request :session :id))]
    (update-session! request (fn [a b] (update-in a [id] #(dissoc % k))) nil)))

;;
;; Validation
;; ==========
;;

(defn invalid? [name validation-fn props form-data request]
  (if-let [errors (validation-fn props form-data)]
    (do (set-flash-value! name
                          (merge {:form-data form-data} errors)
                          request)
        true)
    false))

(defn required-field [m k msg]
  (if (empty? (k m)) {k msg}))

;;
;; Tables
;; =====
;;

(defn get-filters-from-url [url]
  {})

(defn get-sort-and-page-from-url [url]
  (reduce
   (fn [a b]
     (merge-with concat a
            (let [command (first b)
                  params (last b)]
              (cond (= command "/sort")
                    {:sort [(keyword (first params))
                            (keyword (last params))]}
                    (= command "/page")
                    {:page [(first params) (last params)]}
                    :else {}))))
   {}
   (map #(vector (first %)
                 (rest (re-split #"/" (last %))))
        (partition 2
                   (rest
                    (re-partition #"/sort|/filter|/page" url))))))

(defn merge-table-state-vecs [old new]
  (let [order (distinct
               (concat (map first (partition 2 old))
                       (map first (partition 2 new))))
        m (reduce (fn [a b]
                       (assoc a (first b) (last b)))
                     {}
                     (partition 2 (concat old new)))]
    (vec
     (apply concat
            (filter #(not (= :remove (last %)))
                    (map #(vector % (m %)) order))))))

(defn update-table-state! [table-name request]
  (let [id (keyword (-> request :session :id))
        params (:params request)]
    (-> (update-session!
         request
         (fn [a b]
           (let [current-state (or (-> a id :table-state table-name) {})]
             (-> a
                 (assoc-in [id :table-state table-name]
                           (merge-with merge-table-state-vecs
                                       current-state
                                       b)))))
         (merge
          (hash-map :sort
                    (vec (concat
                          (if-let [sort (:sort-asc params)]
                            [(keyword sort) :asc])
                          (if-let [sort (:sort-desc params)]
                            [(keyword sort) :desc])
                          (if-let [sort (:remove-sort params)]
                            [(keyword sort) :remove]))))
          (hash-map :filter
                    (vec (concat
                          (if-let [filter (:filter params)]
                            [(keyword filter) (:filter-value params)])
                          (if-let [filter (:remove-filter params)]
                            [(keyword filter) :remove]))))))
        id :table-state table-name)))

(defn build-page-and-sort-map [table-state-map]
  (assoc {} :sort
         (vec
          (apply concat
                 (map #(list (last %) (name (first %)))
                      (partition 2 (:sort table-state-map)))))))

(defn build-filter-map [table-state-map]
  (reduce (fn [a b] (assoc a (first b) (last b)))
          {}
          (partition 2 (:filter table-state-map))))

(defn current-filters! [table-name request]
 (let [t-state (update-table-state! table-name request)]
    (build-filter-map t-state)))

(defn current-page-and-sort! [table-name request]
  (let [t-state (update-table-state! table-name request)]
    (build-page-and-sort-map t-state)))

(defn get-column-name [column-spec-row]
  (if (keyword? column-spec-row)
    column-spec-row
    (:column column-spec-row)))

(defn table-cell [map-or-value & values]
  (vec
   (filter #(not (nil? %))
           (if (map? map-or-value)
             [:td (:attr map-or-value)
              (if (contains? (:actions map-or-value) :filter)
                [:a {:href (str "?filter=" (name (:column map-or-value))
                                "&filter-value="
                                (url-encode (:value map-or-value)))}
                 (:value map-or-value)]
                (:value map-or-value))]
             (if (seq values)
               (vec (concat [:td map-or-value] values))
               [:td map-or-value])))))

(defn table-row
  ([coll] [:tr (map table-cell coll)])
  ([coll row-class]
     [:tr {:class row-class} (map table-cell coll)]))

(defn table-header [coll]
  [:tr (map #(vector :th {:nowrap ""} %) coll)])

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
      (vec
       (concat
        [:table]
        rows)))))

(defn standard-table [props columns column-fn data]
  [:table {:class "list"}
      (table-header (map #(if-let [p (props %)] p %) columns))
   (map
    (fn [row-data class]
      (table-row (map #(column-fn % row-data) columns) class))
    data (cycle ["odd" "even"]))])

(defn get-table-state [request table-name]
  (let [id (keyword (-> request :session :id))
        t-state (-> @session id :table-state table-name)]
    t-state))

(defn opposite-sort-dir [d]
  (cond (= d :asc) :desc
        (= d :desc) :asc
        :else d))

(defn table-column-names [column-spec]
  (map get-column-name column-spec))

(defn table-sort-columns [column-spec]
  (set
   (map #(if (contains? (:actions %) :sort) (:column %))
        (filter map? column-spec))))

(defn sort-table-header [request table-name props column-spec]
  (let [t-state (:sort (get-table-state request table-name))
        sort-dir-map (reduce
                      (fn [a b]
                        (assoc a (first b) (last b)))
                      {}
                      (partition 2 t-state))
        sort-columns (table-sort-columns column-spec)]
    [:tr (map
          #(let [sort-dir (sort-dir-map %)]
             (vector :th {:nowrap ""}
                     (if (contains? sort-columns %)
                       (link-to (str "?sort-"
                                  (name (if-let [sd (opposite-sort-dir
                                                     sort-dir)]
                                          sd
                                          :asc))
                                  "="
                                  (name %))
                                (props %))
                       (props %))
                   "&nbsp;"
                   (cond (= sort-dir :asc) (image "sort_ascending.png")
                         (= sort-dir :desc) (image "sort_descending.png")
                         :else (image "blank16.gif"))))
          (table-column-names column-spec))]))

(defn- create-saf-table-control [t-state k title link-fn data-fn]
  (let [t-state (k t-state)]
      (if (seq t-state)
        (vec
         (concat
          [:div title]
          (interpose
           ", "
           (apply vector (map link-fn (data-fn t-state))))))
        "")))

(defn create-table-sort-and-filter-controls [request table-name props]
  (let [current-state (get-table-state request table-name)]
    (vec
    (conj
     [:div {:class "filter-and-sort-controls"}]
     (create-saf-table-control current-state :sort "Remove sort: "
                               #(vector :a
                                        {:href (str "?remove-sort=" (name %))}
                                        ((keyword %) props %))
                               #(map first (partition 2 %)))
     (create-saf-table-control current-state :filter "Remove filter: "
                               #(vector :a
                                        {:href
                                         (str "?remove-filter="
                                              (name (first%)))}
                                        ((keyword (first %)) props (first %))
                                        " = "
                                        (last %))
                               #(partition 2 %))))))

(defn filter-and-sort-table [request t-spec column-spec cell-fn data-fn]
  (let [props (:props t-spec)
        t-name (:name t-spec)
        table-data (data-fn (:type t-spec)
                            (current-filters! t-name request)
                            (current-page-and-sort! t-name request))
        columns (table-column-names column-spec)]
    [:div {:class "filter-and-sort-table"}
   (create-table-sort-and-filter-controls request t-name props)
   [:table {:class "list"}
      (sort-table-header request t-name props column-spec)
   (map
    (fn [row-data class]
      (table-row (map #(hash-map
                        :column (get-column-name %)
                        :value (cell-fn (get-column-name %) row-data)
                        :attr {:align (if-let [a (:align %)] a :left)}
                        :actions (:actions %))
                      column-spec)
                 class))
    table-data
    (cycle ["odd" "even"]))]]))

;;
;; Forms
;; =====
;;

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

(defn form-cancelled [params]
  (= "Cancel" (:cancel params)))

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
                   (if (and (contains? cb-set (key b))
                            (= "checkbox-true" (val b)))
                     (assoc a (key b) "Y")
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
  (let [v (k params)
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
     (if-let [form-state (get-flash-value! form-name request)]
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

;;
;; Editing Simple Lists
;; ====================
;;

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

(defn list-editor [paged-list-fn find-by-id-fn props finished
                   type request params]
  (let [action (params :*)]
    (cond (.startsWith action "/list")
          (list-editor-display-list paged-list-fn type props finished)
          (.startsWith action "/add")
          (list-editor-form find-by-id-fn type props request)
          (.startsWith action "/edit")
          (list-editor-form find-by-id-fn type props request (params :id))
          (.startsWith action "/delete")
          (confirm-delete find-by-id-fn type props (params :id))
          :else "This action is not implemented...")))

(defn validate-list-item [props form-data]
  (required-field form-data :name "Please enter a name."))

(defn save-list-item [save-fn type form-data action request]
  (cond (invalid? type validate-list-item {} form-data request) action
         :else (do (save-fn form-data)
                   "list")))

(defn list-updater [save-fn delete-by-id-fn type request params]
  (let [action (params :*)]
    (redirect-to
     (cond (form-cancelled params) "list"
           (.startsWith action "/add")
           (save-list-item save-fn
                           type
                           {:type type :name (params :name)}
                           "add"
                           request)
           (.startsWith action "/edit")
           (save-list-item save-fn
                           type
                           {:type type :name (params :name) :id (params :id)}
                           "edit"
                           request)
           (.startsWith action "/delete")
           (do
             (delete-by-id-fn type (params :id))
             "list")
           :else "list"))))

;;
;; Standard Pages
;; ==============
;;

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
          raising an Authentication error but the user has been authenticated."]
   [:br]])
