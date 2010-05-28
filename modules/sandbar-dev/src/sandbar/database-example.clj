;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.database-example
  (:use (sandbar database)))

(def db {:connection {:classname "com.mysql.jdbc.Driver"
                      :subprotocol "mysql"
                      :subname "//localhost/sandbar_sample_db"
                      :user "sample_db_user"
                      :password "123456789"}})

(def dm
     (model
      (relation :album [:id :title]
                [:has-many :artists :artist
                 :=> :album_artist :album_id :artist_id])
      (relation :artist [:id :name])))

(def $ (partial query db dm))
(def $1 (partial query-1 db dm))

;; My dream query language
($ :album) ;; return only albums
;; select * from album
($1 :album {:name "Magic Potion"}) ;; return only albums
;; select * from album where name = "Magic Potion"
($ :album {:name "Magic Potion"} {:name "Let's*"}) ;; "or" query
;; select * from album where name = "Magic Potion" or name like "let's%"
($1 :album [:id] {:name "Magic Potion"})
;; select id from album where name = "Magic Potion"

($ :album :with :artists) ;; return albums with nested artists
($ :artists :with :albums) ;; return artists with nested albums

($ :album :with [:artists [:id] {:name "David*"}])

