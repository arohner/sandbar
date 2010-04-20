;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.user-tables-mysql
  (:use (sandbar auth util
                 [database :as database])))

;; Create user tables in mysql. To create these tables, load this file
;; and then run (create-tables).

(defn get-connection []
  {:connection
   {:classname "com.mysql.jdbc.Driver"
    :subprotocol "mysql"
    :subname "//localhost/idea_db"
    :user "idea_user"
    :password "123456789"}})

(defn- create-user-table [create-table-fn]
  (create-table-fn
    "CREATE TABLE app_user 
       (id bigint(20) NOT NULL AUTO_INCREMENT, 
        username varchar(255) NOT NULL,
        password varchar(255) NOT NULL,
        salt varchar(255) NOT NULL,
        first_name varchar(255) NOT NULL,
        last_name varchar(255) NOT NULL,
        email varchar(255) NOT NULL,
        account_enabled char(1) NOT NULL,
        PRIMARY KEY (id),
        UNIQUE KEY email (email),
        UNIQUE KEY username (username)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8;"))

(defn- create-role-table [create-table-fn]
  (create-table-fn
   "CREATE TABLE role
      (id bigint(20) NOT NULL AUTO_INCREMENT,
       name varchar(255) NOT NULL,
       PRIMARY KEY (id),
       UNIQUE KEY name (name)
       ) ENGINE=InnoDB DEFAULT CHARSET=utf8;"))

(defn- create-role-user-table [create-table-fn]
  (create-table-fn
   "CREATE TABLE user_role
      (id bigint(20) NOT NULL AUTO_INCREMENT,
       role_id bigint(20) NOT NULL,
       user_id bigint(20) NOT NULL,
       PRIMARY KEY (id),
       KEY FK_user_key (user_id),
       KEY FK_role_key (role_id),
       CONSTRAINT FK_user_key FOREIGN KEY (user_id)
         REFERENCES app_user (id),
       CONSTRAINT FK_role_key FOREIGN KEY (role_id)
         REFERENCES role (id)
       ) ENGINE=InnoDB DEFAULT CHARSET=utf8;"))

(defn add-role [insert-fn role]
  (insert-fn :role
             {:name (name role)}))

(defn insert-role-records [insert-fn]
  (insert-fn :role {:name "admin"})
  (insert-fn :role {:name "user"}))

(defn insert-user-records [insert-fn]
  (let [salt (random-string 24 24)]
    (insert-fn :app_user {:username "admin-user"
                         :password (hash-password "admin" salt)
                         :salt salt
                         :first_name "Admin"
                         :last_name "User"
                         :email "admin-user@test.com"
                          :account_enabled "Y"}))
  (let [salt (random-string 24 24)]
    (insert-fn :app_user {:username "normal-user"
                         :password (hash-password "password" salt)
                         :salt salt
                         :first_name "Normal"
                         :last_name "User"
                         :email "normal-user@test.com"
                         :account_enabled "Y"})))

(defn assoc-users-with-roles [insert-fn]
  (insert-fn :user_role {:user_id 1 :role_id 1})
  (insert-fn :user_role {:user_id 2 :role_id 2}))

(defn create-tables
  ([] (create-tables nil))
  ([drop]
     (let [db (get-connection)
           drop-fn (partial database/db-drop-table db)
           insert-fn (partial database/db-insert db)
           create-table-fn (partial database/db-do-commands db)] 
       (do
         (if drop
           (do (drop-fn :user_role)
               (drop-fn :role)
               (drop-fn :app_user)))
         (create-user-table create-table-fn)
         (create-role-table create-table-fn)
         (create-role-user-table create-table-fn)
         (insert-role-records insert-fn)
         (insert-user-records insert-fn)
         (assoc-users-with-roles insert-fn)))))

