(ns sandbar.sample-database-migrations
  (:require [sandbar.database :as database]))

(defn create-key-value-table [create-table-fn]
  (create-table-fn
    "create table key_value 
       (id bigint not null auto_increment,
        key_name varchar(255) not null,
        value varchar(255) not null,
        primary key (id)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8;"))

(defn migration-20100524000 []
  {:forward (fn [db] nil)})

(defn drop-table [table]
  (fn [db]
    (database/db-do-commands
     db
     (str "DROP TABLE " (name table)))))

(defn create-table [table & spec]
  (fn [db]
    (database/db-do-commands
     db
     (str
      "CREATE TABLE " (name table)
      " (id bigint(20) NOT NULL AUTO_INCREMENT,"
         (apply str spec)
         "PRIMARY KEY (id)
         ) ENGINE=InnoDB DEFAULT CHARSET=utf8;"))))

(defmulti col (fn [_ type & spec] type))

(defmethod col :string [col-name type & spec]
  (str (name col-name) " varchar(255)"
       (if (contains? (set spec) :not-null)
         " NOT NULL"
         "")
       ","))

(defmethod col :id [col-name type & spec]
  (str (name col-name) " bigint(20)"
       (if (contains? (set spec) :not-null)
         " NOT NULL"
         "")
       ","))

(defn constraint
  ([this-col _ table that-col]
     (constraint this-col table that-col))
  ([this-col table that-col]
     (let [this-name (name this-col)
           foreign-key (str "FK_" this-name "_key")]
       (str "KEY " foreign-key " (" this-name "),"
            "CONSTRAINT " foreign-key " FOREIGN KEY (" this-name ") "
            "REFERENCES " (name table) " (" (name that-col) "),"))))

(defn migration-20100524001 []
  {:forward
   (create-table :album
                 (col :title :string :not-null))
   :back
   (drop-table :album)})

(defn migration-20100525000 []
  {:forward
   (create-table :artist
                 (col :name :string :not-null))
   :back
   (drop-table :artist)})

(defn migration-20100525001 []
  {:forward
   (create-table :album_artist
                 (col :album_id :id :not-null)
                 (col :artist_id :id :not-null)
                 (constraint :album_id :=> :album :id)
                 (constraint :artist_id :=> :artist :id))
   :back
   (drop-table :album_artist)})
