; Copyright (c) Brenton Ashworth. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file COPYING at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns sandbar.database
  (:use (clojure.contrib [sql :as sql]
	                 [java-utils :only (as-str)]
	                 [str-utils :only (re-gsub)])))

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

(defn- split-criteria
  "Create a sequence where the first element is the vector of keys and the
   remaining elements are the values"
  [criteria]
  (cons (vec (keys criteria)) (vals criteria)))

(defn- create-comparison
  [key value]
  (if (nil? value)
      (str (as-str key) " IS NULL")
      (if (. (as-str value) endsWith "*")
          (str (as-str key) " like ?")
          (str (as-str key) " = ?"))))

(defn- create-where-str
  "Create a where string out of the keys from a criteria map"
  [keys values]
  (apply str (interpose " and " (map create-comparison keys values))))

(defn- replace-wildcard [s]
  (if (. (as-str s) endsWith "*")
      (re-gsub #"[*]" "%" s)
      s))

(defn- create-where-vec
  "Create a vector where the first element is the where query and the
   remaining elements are the parameters"
  [criteria]
  (if (> (count criteria) 0)
  (let [split (split-criteria criteria)]
    (cons
      (create-where-str (first split) (rest split))
      (map replace-wildcard (filter #(not (nil? %)) (rest split)))))
  nil))

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

(defn- create-select
  "Create the select vector that can be passed to with-query-results"
  [name criteria params]
  (let [select-part (str "SELECT * FROM " (as-str name))
        where-part (create-where-vec criteria)
        order-by-part (create-order-by params)]
    (if where-part
      (vec (cons (str select-part " WHERE " (first where-part) order-by-part)
                 (rest where-part)))
      [(str select-part order-by-part)])))

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

(defn find-in
  "Find records in a table based on the passed criteria"
  ([db name] (find-in db name {} {}))
  ([db name criteria] (find-in db name criteria {}))
  ([db name criteria params]
     (sql/with-connection (:connection db)
       (sql/with-query-results res
         (create-select name criteria params)
         (map #(assoc % :type name) (into [] res))))))

(defn delete-record [db table rec]
  (with-transaction db 
	#(sql/delete-rows table
       (vec (create-where-vec {:id (:id rec)})))))
							
(defn save-or-update [db record]
  (let [table (:type record)]
    (if (:id record)
      (db-update db table (:id record) (m-dissoc record :id :type))
      (db-insert db table (m-dissoc record :type)))))

