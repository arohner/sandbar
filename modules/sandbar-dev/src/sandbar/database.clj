;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.database
  "Very simple layer between the application and clojure.contrib.sql."
  (:use (clojure.contrib [sql :as sql]
	                 [java-utils :only (as-str)]
	                 [str-utils :only (re-gsub)]
                         [map-utils :only (deep-merge-with)])))

;; Begin define relationships

(defn model [& body]
  {:model (apply deep-merge-with merge body)})

(defn compile-has-many [relation coll]
  (cond (= (count coll) 8)
        (let [[_ alias many-relation name _ link from to] coll]
          {many-relation {:alias alias
                          :name-fn name}
           relation {:joins
                     {:many-to-many [{:relation many-relation
                                      :alias alias
                                      :link link
                                      :from from
                                      :to to}]}}})))

(defn relation [name & attrs]
  (loop [result {name
                 {:attrs (first attrs)}}
         attrs (rest attrs)]
    (if (seq attrs)
      (recur (deep-merge-with merge
                              result
                              (let [next (first attrs)]
                                (cond (= (first next) :has-many)
                                      (compile-has-many name next)
                                      :else {})))
             (rest attrs))
      result)))

;; End define relationships

(defn map-values [f m]
  (let [keys (keys m)
	values (vals m)]
    (zipmap keys (map f values))))

(defn- with-transaction
  "Execute a function within a transaction"
  [db f]
  (sql/with-connection (:connection db)
    (sql/transaction (f)))
  nil)

(defn split-criteria
  "Create a sequence where the first element is the vector of keys and the
   remaining elements are the values"
  [criteria]
  (cons (vec (keys criteria)) (vals criteria)))

(defn create-comparison
  ([key value]
     (create-comparison nil key value))
  ([prefix key value]
     (str (if prefix
            (str (name prefix) "."))
          (name key)
          (if (nil? value)
            " IS NULL"
            (if (. (as-str value) endsWith "*")
              " like ?"
              " = ?")))))

(defn create-where-str
  "Create a where string out of the keys from a criteria map"
  ([keys values]
     (create-where-str nil keys values))
  ([prefix keys values]
     (apply str (interpose
                 " AND "
                 (map (partial create-comparison prefix) keys values)))))

(defn replace-wildcard [s]
  (if (. (as-str s) endsWith "*")
    (re-gsub #"[*]" "%" s)
    s))

(defn create-where-vec
  "Create a vector where the first element is the where query and the
   remaining elements are the parameters"
  ([criteria]
     (create-where-vec nil criteria))
  ([relation criteria]
     (if (> (count criteria) 0)
       (let [split (split-criteria criteria)]
         (cons
          (create-where-str relation (first split) (rest split))
          (map replace-wildcard (filter #(not (nil? %)) (rest split)))))
       nil)))

(defn to-string [a]
  (cond (keyword? a) (name a)
        :else (.toString a)))

(defn- create-order-by [params]
  (let [sort-vec (:sort params)
        order-by-str (if (seq sort-vec)
                       (reduce
                        (fn [a b]
                          (if (= 1 (count b))
                            (str a (first b))
                            (str a " " (to-string (last b)) " "
                                 (to-string (first b)))))
                        ""
                        (interpose [","] (partition 2 sort-vec))) nil)]
    (if order-by-str (str " order by" order-by-str) "")))

(defn create-relation-qualified-names [relation attrs]
  (map #(let [r (name relation)
              a (name %)]
          (str r "." a " as " r "_" a))
       attrs))

(defn create-attr-list [relation model]
  (if model
    (let [attrs (-> model relation :attrs)]
      (loop [result (create-relation-qualified-names relation
                                                     attrs)
             many-to-many (-> model relation :joins :many-to-many)]
        (if (seq many-to-many)
          (let [many-side (:relation (first many-to-many))
                attrs (-> model many-side :attrs)]
            (recur (concat result (create-relation-qualified-names many-side
                                                                   attrs))
                   (rest many-to-many)))
          (apply str " " (interpose ", " result)))))
    " *"))

(defn create-joins [relation model]
  (loop [result ""
         many-to-many (-> model relation :joins :many-to-many)]
    (if (seq many-to-many)
      (let [{rel :relation link :link from :from to :to} (first many-to-many)]
        (recur
         (let [rel (name rel)
               link (name link)
               from (name from)
               to (name to)]
           (str result
                " LEFT JOIN " link " ON " (name relation) ".id = " link "." from
                " LEFT JOIN " rel " ON " link "." to " = " rel ".id"))
         (rest many-to-many))) 
      result)))

(defmulti create-select (fn [conn _ _ _] (:subprotocol conn)))

(defn create-select-mysql
  "Create the select vector that can be passed to with-query-results"
  [conn relation criteria params]
  (let [model (:model params)
        select-part (str "SELECT" (create-attr-list relation model)
                         " FROM " (as-str relation)
                         (create-joins relation model))
        where-part (create-where-vec relation criteria)
        order-by-part (create-order-by params)]
    (if where-part
      (vec (cons (str select-part " WHERE " (first where-part) order-by-part)
                 (rest where-part)))
      [(str select-part order-by-part)])))

(defmethod create-select "mysql"
  [conn relation criteria params]
  (create-select-mysql conn relation criteria params))

(defmethod create-select :default
  [conn relation criteria params]
  (create-select-mysql conn relation criteria params))

(defn db-drop-table [db name]
  (sql/with-connection (:connection db)
      (sql/drop-table name)))

(defn db-do-commands [db commands]
  (sql/with-connection (:connection db)
    (sql/do-commands commands)))

(defn db-insert
  "Insert records, maps from keys specifying columns to values"
  [db name rec]
  (with-transaction db #(sql/insert-records name rec)))

(defn db-update
  "Update a record"
  [db name id rec]
  (with-transaction db 
	#(sql/update-values name ["id=?" id] rec)))

(defn- m-dissoc [m & keys]
  (apply dissoc (into {} m) keys))

(defn dequalify-joined-map
  "Create a map that has dequalified key names for this relation. The input
   map will have the relation named appended to the front of each key."
  [relation m]
  (let [prefix (name relation)]
    (reduce (fn [a b]
              (let [k (name (key b))]
                (if (.startsWith k prefix)
                  (assoc a (keyword
                            (.substring k (+ 1 (count prefix)))) (val b))
                  a)))
            {}
            m)))

(defn merge-many
  "Create a map from the many side of the association and add it to the
   collection contained in the one side."
  [relation sub-relation alias result-map next-result]
  (let [sub (dequalify-joined-map sub-relation next-result)
        id ((keyword (str (name relation) "_id")) next-result)
        host (get result-map id)]
    (assoc result-map id
           (if (:id sub)
             (merge-with #(vec (concat %1 %2)) host
                         {alias [(assoc sub :type sub-relation)]})
             (assoc host alias [])))))

(defn create-order-and-results-map
  "In one pass through the results, create a vector containing the order of
   the results by id and a map with the unique results for this relation."
  [relation results]
  (reduce
   (fn [a b]
     (let [m (dequalify-joined-map relation b)]
       [(conj (first a) (:id m))
        (assoc (last a) (:id m) m)]))
   [[] {}]
   results))

(defn transform-join-results
  "Transform flat join results into a hierarchy of maps."
  [relation model results]
  (let [[order rec-map] (create-order-and-results-map relation results)]
    (loop [many-to-many (-> model relation :joins :many-to-many)
           rec-map rec-map]
      (if (seq many-to-many)
        (let [next (first many-to-many)
              sub-relation (:relation next)
              alias (:alias next)]
          (recur
           (rest many-to-many)
           (reduce (partial merge-many relation sub-relation alias)
                   rec-map
                   results)))
        (vec (map #(get rec-map %) (distinct order)))))))

(defn raw-sql
  "Run a query using raw sql of the form
   [\"SELECT * FROM table WHERE name = ? and age = ?\" \"joe\" 42]."
  [db sql]
  (sql/with-connection (:connection db)
    (sql/with-query-results res
      sql
      (into [] res))))

(defn query
  "Find records in a table based on the passed criteria"
  ([db name-or-sql]
     (if (keyword? name-or-sql)
       (query db name-or-sql {} {})
       (raw-sql db name-or-sql)))
  ([db name criteria] (query db name criteria {}))
  ([db name criteria params]
     (let [conn (:connection db)]
       (sql/with-connection conn
         (sql/with-query-results res
           (create-select conn name criteria params)
           (map #(assoc % :type name)
                (let [results (into [] res)]
                  (if-let [model (:model params)] 
                    (transform-join-results name model results)
                    results))))))))

(defn delete-record [db table rec]
  (with-transaction db 
	#(sql/delete-rows table
       (vec (create-where-vec {:id (:id rec)})))))
							
(defn save-or-update [db record]
  (let [table (:type record)]
    (if (:id record)
      (db-update db table (:id record) (m-dissoc record :id :type))
      (db-insert db table (m-dissoc record :type)))))


(comment

  ;; various kinds of queries
  (query db :person)
  (query db :person {:id 8})
  (query db :person {:name "brent*"})

  ;; Arbitrary SQL
  (query db ["SELECT * FROM person"])
  (query db ["SELECT * FROM person WHERE id = ?" 8])

  ;; using data models
  (def data-model 
       (model (relation :car [:id :make])
              (relation :person [:id :first_name :last_name]
                        [:has-many :cars :car :make
                         :through :person_car :person_id :car_id])))
  
  (query db :person {} data-model)
  ;; The above query will perform a join through the :person_car table
  ;; and will get all people, each one have a :cars key that contains
  ;; a list of associated cars.
  
  (query db :person {:id 8} data-model)
  )
