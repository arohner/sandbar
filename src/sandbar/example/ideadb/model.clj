; Copyright (c) Brenton Ashworth. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns sandbar.example.ideadb.model
  (:use [sandbar.database
         :only (find-in delete-record save-or-update)]
        [clojure.contrib.str-utils :only (re-split re-partition)]))

(def db (atom nil))

(defn request-att [request name]
  (if-let [sr (:servlet-request request)]
    (.getAttribute sr name)))

(defn get-connection-info [request]
  {:connection
   (-> {}
       (assoc :classname
         (or (request-att request "classname")
             "com.mysql.jdbc.Driver"))
       (assoc :subprotocol
         (or (request-att request "subprotocol")
             "mysql"))
       (assoc :subname
         (or (request-att request "subname")
             "//localhost/idea_db"))
       (assoc :user
         (or (request-att request "user")
             "idea_user"))
       (assoc :password
         (or (request-att request "password")
             "123456789")))})

(defn configure-database [request]
  (if (not @db)
    (swap! db (fn [a b] b) (get-connection-info request))))

(defn get-list [type]
  (let [the-list (find-in @db type)]
    (sort-by :name the-list)))

(defn filter-and-sort-records
  ([type]
     (filter-and-sort-records type {} {}))
  ([type filters sort-and-page]
     (find-in @db type filters sort-and-page)))

(defn find-by-id [type id]
  (first (find-in @db type {:id id})))

(defn delete-by-id [type id]
  (if-let [record (find-by-id type id)]
    (delete-record @db type record)))

(defn delete-rec [rec]
  (delete-record @db (:type rec) rec))

(defn paged-list [type page-params]
  (let [the-list (find-in @db type)]
    (sort-by :name the-list)))

(defn save [m]
  (save-or-update @db m))

