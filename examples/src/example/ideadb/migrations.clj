;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns example.ideadb.migrations
  (:require [sandbar.database :as database])
  (:use (clojure.contrib [seq-utils :only (flatten)])
        (example.ideadb [data :only (get-connection-info)])))

;; TODO - Add library support and update this file so that you can
;; easily add new migration functions when the database changes.

;;
;; Database configuration. To Setup the database, load this file and
;; then run (create-tables).
;;

(defn mysql-db [] {:connection (get-connection-info {})})

;;
;; Table Schema
;; ============
;;

(defn create-list-table [t-name create-table-fn]
  (create-table-fn
   (str "create table " t-name
        "(id bigint not null auto_increment, 
	name varchar(255) not null,
	primary key (id)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8;")))

(defn create-idea-category-table [create-table-fn]
  (create-list-table "idea_category" create-table-fn))

(defn create-idea-type-table [create-table-fn]
  (create-list-table "idea_type" create-table-fn))

(defn create-business-unit-table [create-table-fn]
  (create-list-table "business_unit" create-table-fn))

(defn create-idea-status-table [create-table-fn]
  (create-list-table "idea_status" create-table-fn))

(defn create-idea-table [create-table-fn]
  (create-table-fn
    "create table idea 
       (id bigint not null auto_increment, 
        name varchar(255) not null,
        description text not null, 
        customer_need text not null,
        originator varchar(255),
        user_id varchar(32) not null,
        date_entered varchar(10) not null,
        category varchar(255),
        idea_type varchar(255),
        business_unit varchar(255),
        status varchar(255),
        archive char(1),
        primary key (id)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8;"))

;;
;; Load Default Table Data
;; =======================
;;

(defn insert-category-records [insert-fn db]
  (insert-fn :idea_category {:name "Category 1"})
  (insert-fn :idea_category {:name "Category 2"}))

(defn insert-type-records [insert-fn db]
  (insert-fn :idea_type {:name "Type 1"})
  (insert-fn :idea_type {:name "Type 2"}))

(defn insert-business-unit-records [insert-fn db]
  (insert-fn :business_unit {:name "Business Unit 1"})
  (insert-fn :business_unit {:name "Business Unit 2"}))

(defn insert-status-records [insert-fn db]
  (insert-fn :idea_status {:name "Status 1"})
  (insert-fn :idea_status {:name "Status 2"}))

(defn insert-idea-records [insert-fn db]
  (insert-fn :idea
             {:name "Project Sandbar",
              :description "Libraries which implement useful functionality.", 
              :customer_need "Clojure web devs really want this.",
              :originator "Admin User",
              :date_entered "2010-10-01",
              :category "Category 1",
              :idea_type "Type 1",
              :business_unit "Business Unit 1",
              :status "Status 1",
              :archive "N"
              :user_id "admin-user"}))

;;
;; Create Tables Function
;; ======================
;;

(defn create-tables
  ([] (create-tables nil))
  ([drop]
     (let [db (:connection (mysql-db))
           drop-fn (partial database/db-drop-table db)
           insert-fn (partial database/db-insert db)
           create-table-fn (partial database/db-do-commands db)] 
       (do
         (if drop
           (do (drop-fn :idea)
               (drop-fn :idea_category)
               (drop-fn :idea_type)
               (drop-fn :business_unit)
               (drop-fn :idea_status)))
         (create-idea-category-table create-table-fn)
         (create-idea-type-table create-table-fn)
         (create-business-unit-table create-table-fn)
         (create-idea-status-table create-table-fn)
         (create-idea-table create-table-fn)
         (insert-category-records insert-fn db)
         (insert-type-records insert-fn db)
         (insert-business-unit-records insert-fn db)
         (insert-status-records insert-fn db)
         (insert-idea-records insert-fn db)))))

