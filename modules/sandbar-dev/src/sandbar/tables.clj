;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.tables
  "HTML tables which may be filtered, sorted and paged."
  (:use (ring.util [codec :only (url-encode)])
        (hiccup core page-helpers)
        (sandbar core stateful-session)))

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

(defn update-table-state! [table-name params]
  (-> (update-session!
       (fn [a b]
         (let [current-state (or (-> a :table-state table-name) {})]
           (-> a
               (assoc-in [:table-state table-name]
                         (merge-with merge-table-state-vecs
                                     current-state
                                     b)))))
       (merge
        (hash-map :sort
                  (vec (concat
                        (if-let [sort (get params "sort-asc")]
                          [(keyword sort) :asc])
                        (if-let [sort (get params "sort-desc")]
                          [(keyword sort) :desc])
                        (if-let [sort (get params "remove-sort")]
                          [(keyword sort) :remove]))))
        (hash-map :filter
                  (vec (concat
                        (if-let [filter (get params "filter")]
                          [(keyword filter) (get params "filter-value")])
                        (if-let [filter (get params "remove-filter")]
                          [(keyword filter) :remove]))))))
      :table-state table-name))

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

(defn current-filters! [table-name params]
 (let [t-state (update-table-state! table-name params)]
    (build-filter-map t-state)))

(defn current-page-and-sort! [table-name params]
  (let [t-state (update-table-state! table-name params)]
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



(defn standard-table [props columns column-fn data]
  [:table {:class "list"}
      (table-header (map #(if-let [p (props %)] p %) columns))
   (map
    (fn [row-data class]
      (table-row (map #(column-fn % row-data) columns) class))
    data (cycle ["odd" "even"]))])

(defn get-table-state [table-name]
  (session-get [:table-state table-name]))

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

(defn sort-table-header [table-name props column-spec]
  (let [t-state (:sort (get-table-state table-name))
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

(defn create-table-sort-and-filter-controls [table-name props]
  (let [current-state (get-table-state table-name)]
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

(defn filter-and-sort-table [params t-spec column-spec cell-fn data-fn]
  (let [props (:props t-spec)
        t-name (:name t-spec)
        table-data (data-fn (:type t-spec)
                            (current-filters! t-name params)
                            (current-page-and-sort! t-name params))
        columns (table-column-names column-spec)]
    [:div {:class "filter-and-sort-table"}
     (create-table-sort-and-filter-controls t-name props)
     [:table {:class "list"}
      (sort-table-header t-name props column-spec)
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

