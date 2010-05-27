;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.migrations
  "Very simple database migration functionality."
  (:use (clojure.contrib [str-utils :only (re-split)]
                         [ns-utils :only (ns-vars)])
        (sandbar [autorouter :only (load-namespace)]
                 database)))

;; You might want to move load-namespace somewhere else. It doesn't
;; make sense for this to depend on autorouter.

(defn str->int
  "Convert a string to a BigInteger."
  [s]
  (java.math.BigInteger. s))

(defn find-all-migrations
  "Find the sequence of migration ids from the given namespace."
  [ns]
  (if-let [v (and (load-namespace ns)
                  (ns-vars (symbol ns)))]
    (vec
     (map #(last (re-split #"-" (name %)))
          (filter #(.startsWith (name %) "migration-") v)))))

(defn run-migration
  "Run a single migration function. If the loaded function returns a non-nil
   value then return it otherwise return true. If the function throws an
   exception then return false."
  [db ns id direction]
  {:pre [(direction #{:forward :back})]}
  (let [m-fn (ns-resolve (symbol ns)
                         (symbol (str "migration-" id)))
        m (m-fn)]
    (if-let [mig-fn (direction m)]
      (try
       (let [result (mig-fn db)]
         (if (not (nil? result))
           result
           true))
       (catch Exception _ false))
      false)))

(defn run-migration-ids
  "Dynamically load and attempt to run all of the migrations which have ids
   that match those in the list. Run the migrations in numeric order. Returns
   a map of ids to result values that are either true or false. Will stop
   when it gets the first false value."
  ([db ns ids] (run-migration-ids db ns ids :forward))
  ([db ns ids direction]
     {:pre [(direction #{:forward :back})]}
     (let [ids (sort (fn [a b] (compare (str->int a) (str->int b)))
                     ids)]
       (loop [status []
              remaining-ids ids]
         (if (and (seq remaining-ids) (or (empty? status) (last status)))
           (recur
            (conj status
                  (run-migration db ns (first remaining-ids) direction))
            (rest remaining-ids))
           (if (not (last status))
             (take (- (count status) 1) ids)
             (take (count status) ids)))))))

(defn get-version-record [db]
  (first
   (query db :key_value {:key_name "database-version"})))

(defn get-version [db]
  (:value
   (get-version-record db)))

(defn set-version [db version]
  (if version
    (let [version-record (get-version-record db)]
      (save-or-update db (assoc version-record :value version)))))

(defn run-all-migrations-after
  "Attempt to run all of the migrations which have a higher id than the id
   number passed to this function. Return a map that shows the success or
   failure of each migration that was run. Set version in the data store to
   be the highest migration number that was successful."
  [db ns v]
  (let [migration-ids (->> ns
                           find-all-migrations
                           (filter #(> (str->int %) v)))]
    (if-let [result (run-migration-ids db ns migration-ids)]
      (do
        (set-version db (last result))
        result))))

(defn rollback-last-migration
  "Rollback the migration with id v. If the rollback is successful then set
   the current value of version to the previous value. Returns a map that
   shows the success or failure of each migration."
  [db ns v]
  (let [last-and-previous (->> ns
                               find-all-migrations
                               (filter #(<= (str->int %) v))
                               reverse
                               (take 2))
        rollback-id (first last-and-previous)
        new-id (second last-and-previous)]
    (if new-id
      (try
       (let [result (run-migration-ids db ns [rollback-id] :back)]
         (if (not (empty? result))
           (set-version db new-id))
         result)
       (catch Exception _ nil)))))



(defn migrate
  "Get the current version of the database and then find and run all
   migrations after that version. Only run migrations until you either run
   out or try to run one that doesn't work."
  ([db mig-ns] (migrate db mig-ns run-all-migrations-after))
  ([db mig-ns mig-fn]
     (if-let [version (get-version db)]
       (mig-fn db mig-ns (str->int version)))))

(defn rollback
  "Rollback the last migration."
  [db migrations-ns]
  (migrate db migrations-ns rollback-last-migration))
