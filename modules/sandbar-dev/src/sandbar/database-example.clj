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
                      :subname "//localhost/fpl_db"
                      :user "tomcat"
                      :password "123456789"}})

(def data-model 
     (model
      (relation :quick_category [:id
                                 :name])
      (relation :quick_page_version [:id
                                     :quick_page
                                     :version
                                     :mod_date
                                     :mod_user_name
                                     :content
                                     :published])
      (relation :quick_page [:id
                             :name
                             :context
                             :current_version
                             :publish_date]
                [:has-many :categories :quick_category
                 :through :quick_page_category :page_id :category_id]
                #_[:has-many :versions :quick_page_version :quick_page])))

(query db :quick_page {} data-model)
