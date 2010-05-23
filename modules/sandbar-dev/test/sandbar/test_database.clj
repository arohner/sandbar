;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.test-database
  (:use (clojure test)
        (sandbar test database)))

(deftest test-map-values
  (is (= (map-values #(if (nil? %) 42 %) {:name "John" :age 27 :height nil}) 
         {:name "John" :age 27 :height 42})))

(deftest test-split-criteria
  (is (= (split-criteria {:id 1}) (list [:id] 1)))
  (is (= (split-criteria {:id 1 :name "John"}) (list [:id :name] 1 "John"))))

(deftest test-create-comparison
  (is (= (create-comparison :id 1) "id = ?"))
  (is (= (create-comparison :name "John") "name = ?"))
  (is (= (create-comparison :name nil) "name IS NULL"))
  (is (= (create-comparison :name "Joh*") "name like ?")))

(deftest test-create-where-str
  (is (= (create-where-str [:id] [1]) "id = ?"))
  (is (= (create-where-str [:id :name] [1 "John"]) "id = ? AND name = ?"))
  (is (= (create-where-str [:id :name :desc :cost] [1 "John" "Something*" nil]) 
         "id = ? AND name = ? AND desc like ? AND cost IS NULL")))

(deftest test-replace-wildcard
  (is (= (replace-wildcard "Something*") "Something%")))

(def model-fixture-many-to-many
     (model
      (relation :category [:id :name])
      (relation :page [:id :name :current_version]
                [:has-many :categories :category
                 :through :page_category :page_id :category_id])))

(def model-fixture-one-to-many
     (model
      (relation :version [:id :content])
      (relation :page [:id :name :current_version]
                [:has-many :versions :version :page_id])))

(def model-fixture-one-and-many-to-many
     (model
      (relation :category [:id :name])
      (relation :version [:id :content])
      (relation :page [:id :name :current_version]
                [:has-many :categories :category
                 :through :page_category :page_id :category_id]
                [:has-many :versions :version :page_id])))

(deftest test-model
  (t "test creating a model"
     (t "containing a many-to-many relationship"
        (is (= model-fixture-many-to-many
               {:model
                {:category {:attrs [:id :name]
                            :alias :categories}
                 :page {:attrs [:id :name :current_version]
                        :joins #{{:type :many-to-many
                                  :relation :category
                                  :alias :categories
                                  :link :page_category
                                  :from :page_id
                                  :to :category_id}}}}})))
     (t "containing a one-to-many relationship"
        (is (= model-fixture-one-to-many
               {:model
                {:version {:attrs [:id :content]
                           :alias :versions}
                 :page {:attrs [:id :name :current_version]
                        :joins #{{:type :one-to-many
                                  :relation :version
                                  :alias :versions
                                  :link :page_id}}}}})))
     (t "containing both one and many -to-many"
        (is (= model-fixture-one-and-many-to-many
               {:model
                {:category {:attrs [:id :name]
                            :alias :categories}
                 :version {:attrs [:id :content]
                           :alias :versions}
                 :page {:attrs [:id :name :current_version]
                        :joins #{{:type :many-to-many
                                  :relation :category
                                  :alias :categories
                                  :link :page_category
                                  :from :page_id
                                  :to :category_id}
                                 {:type :one-to-many
                                  :relation :version
                                  :alias :versions
                                  :link :page_id}}}}})))))

(deftest test-create-attr-list
  (is (= (create-attr-list :page nil)
         " *"))
  (is (= (create-attr-list :page (:model model-fixture-one-and-many-to-many))
         " page.id as page_id, page.name as page_name, page.current_version as page_current_version, category.id as category_id, category.name as category_name, version.id as version_id, version.content as version_content")))

(def many-to-many-join-query
     (str "SELECT page.id as page_id, page.name as page_name, "
          "page.current_version as page_current_version, "
          "category.id as category_id, category.name as category_name "
          "FROM page "
          "LEFT JOIN page_category ON page.id = page_category.page_id "
          "LEFT JOIN category ON page_category.category_id = category.id"))

(def one-to-many-join-query
     (str "SELECT page.id as page_id, page.name as page_name, "
          "page.current_version as page_current_version, "
          "version.id as version_id, version.content as version_content "
          "FROM page "
          "LEFT JOIN version ON page.id = version.page_id"))

(def one-and-many-to-many-join-query
     (str "SELECT page.id as page_id, page.name as page_name, "
          "page.current_version as page_current_version, "
          "category.id as category_id, category.name as category_name, "
          "version.id as version_id, version.content as version_content "
          "FROM page "
          "LEFT JOIN page_category ON page.id = page_category.page_id "
          "LEFT JOIN category ON page_category.category_id = category.id "
          "LEFT JOIN version ON page.id = version.page_id"))

(def select-query "SELECT * FROM page")

(deftest test-create-selects
  (are [a b c] (= (create-selects {:subprotocol "mysql"} :page a b) c)
       {} {} [[select-query]]
       
       {:id 1} {} [[(str select-query " WHERE page.id = ?") 1]]
       
       {:name "brent*"} {}
       [[(str select-query " WHERE page.name like ?") "brent%"]]
       
       {} model-fixture-many-to-many [[many-to-many-join-query]]
       
       {:id 1} model-fixture-many-to-many
       [[(str many-to-many-join-query " WHERE page.id = ?") 1]]

       {:id 1 :name "brenton"} model-fixture-many-to-many
       [[(str many-to-many-join-query
              " WHERE page.id = ? AND page.name = ?") 1 "brenton"]]

       {} model-fixture-one-to-many [[one-to-many-join-query]]

       {} model-fixture-one-and-many-to-many
       [[one-and-many-to-many-join-query]]))

(deftest test-dequalify-joined-map
  (are [x y z] (= (dequalify-joined-map model-fixture-many-to-many x y) z)
       :page
       {:page_id 1 :page_name "one" :category_id 2}
       {:id 1 :name "one"}

       :category
       {:page_id 1 :page_name "one" :category_id 2 :category_name "two"}
       {:id 2 :name "two"}))

(def fixture-join-flat
     [[{:page_id 7 :page_name "one" :page_current_version 1
        :category_id nil :category_name nil} 
       {:page_id 8 :page_name "two" :page_current_version 2
        :category_id 1 :category_name "Clojure"}
       {:page_id 8 :page_name "two" :page_current_version 2
        :category_id 3 :category_name "SICP"}
       {:page_id 9 :page_name "three" :page_current_version 3
        :category_id 1 :category_name "Clojure"}]])

(def fixture-join-nested
     [{:id 7 :name "one" :current_version 1 :categories []
       :sandbar.database/type :page}
      {:id 8 :name "two" :current_version 2 :sandbar.database/type :page
       :categories [{:id 1 :name "Clojure" :sandbar.database/type :category}
                    {:id 3 :name "SICP" :sandbar.database/type :category}]}
      {:id 9 :name "three" :current_version 3 :sandbar.database/type :page
       :categories [{:id 1 :name "Clojure" :sandbar.database/type :category}]}])

(deftest test-transform-query-plan-results
  (t "test transform query plan results"
     (is (= (transform-query-plan-results {:subprotocol "mysql"}
                                        :page
                                        (:model model-fixture-many-to-many)
                                        fixture-join-flat)
          fixture-join-nested))
     (t "with common prefixes"
        (is (= (transform-query-plan-results
                {:subprotocol "mysql"}
                :page
                (:model (model
                         (relation :page_category [:id :name])
                         (relation :page [:id :name]
                                   [:has-many :categories :page_category
                                    :page_id])))
                [[{:page_id 1 :page_name "one" :page_category_id 1
                   :page_category_name "c1"}]])
               [{:sandbar.database/type :page
                 :id 1 :name "one"
                 :categories [{:sandbar.database/type :page_category
                               :id 1 :name "c1"}]}])))))

