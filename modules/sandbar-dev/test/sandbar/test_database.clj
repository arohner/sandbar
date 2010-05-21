;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.test-database
  (:use (clojure test)
        (sandbar database)))

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
	
(def replace-wildcard (ns-resolve 'sandbar.database
                                  'replace-wildcard))

(deftest test-replace-wildcard
  (is (= (replace-wildcard "Something*") "Something%")))

(deftest test-model
  (is (= (model
          (relation :quick_category [:id :name])
          (relation :quick_page [:id :name :context :current_version
                                 :publish_date]
                    [:has-many :quick_category :categories :name
                     :through :quick_page_category :page_id :category_id]))
         {:model
          {:quick_category {:attrs [:id :name]
                            :alias :categories
                            :name-fn :name}
           :quick_page {:attrs [:id :name :context :current_version
                                :publish_date]
                        :joins {:many-to-many [{:relation :quick_category
                                                :alias :categories
                                                :link :quick_page_category
                                                :from :page_id
                                                :to :category_id}]}}}})))

(def model-fixture
     (model
      (relation :quick_category [:id :name])
      (relation :quick_page [:id :name :context :current_version
                             :publish_date]
                [:has-many :quick_category :categories :name
                 :through :quick_page_category :page_id :category_id])))

(def many-to-many-join-query
     "SELECT quick_page.id as quick_page_id, quick_page.name as quick_page_name, quick_page.context as quick_page_context, quick_page.current_version as quick_page_current_version, quick_page.publish_date as quick_page_publish_date, quick_category.id as quick_category_id, quick_category.name as quick_category_name FROM quick_page LEFT JOIN quick_page_category ON quick_page.id = quick_page_category.page_id LEFT JOIN quick_category ON quick_page_category.category_id = quick_category.id")

(def select-query "SELECT * FROM quick_page")

(deftest test-create-select
  (are [a b c] (= (create-select {:subprotocol "mysql"} :quick_page a b) c)
       {} {} [select-query]
       
       {:id 1} {} [(str select-query " WHERE quick_page.id = ?") 1]
       
       {:name "brent*"} {}
       [(str select-query " WHERE quick_page.name like ?") "brent%"]
       
       {} model-fixture [many-to-many-join-query]
       
       {:id 1} model-fixture
       [(str many-to-many-join-query " WHERE quick_page.id = ?") 1]

       {:id 1 :name "brenton"} model-fixture
       [(str many-to-many-join-query
             " WHERE quick_page.id = ? AND quick_page.name = ?") 1 "brenton"]))

(def sample-join-from
     [{:quick_page_id 7, :quick_page_name "site-content",
       :quick_page_context "wiki", :quick_page_current_version 8,
       :quick_page_publish_date nil, :quick_category_id nil,
       :quick_category_name nil} 
      {:quick_page_id 8, :quick_page_name "content-index",
       :quick_page_context "wiki", :quick_page_current_version 46,
       :quick_page_publish_date "2010-05-18", :quick_category_id 1,
       :quick_category_name "Clojure"}
      {:quick_page_id 8, :quick_page_name "content-index",
       :quick_page_context "wiki", :quick_page_current_version 46,
       :quick_page_publish_date "2010-05-18", :quick_category_id 3,
       :quick_category_name "SICP"}
      {:quick_page_id 9, :quick_page_name "content-about",
       :quick_page_context "wiki", :quick_page_current_version 50,
       :quick_page_publish_date "2010-05-18", :quick_category_id 1,
       :quick_category_name "Clojure"}
      {:quick_page_id 10, :quick_page_name "content-tech-about",
       :quick_page_context "wiki", :quick_page_current_version 19,
       :quick_page_publish_date nil, :quick_category_id nil,
       :quick_category_name nil}])

(def sample-join-to
     [{:id 7, :name "site-content", :context "wiki", :current_version 8,
       :publish_date nil, :categories []}
      {:id 8, :name "content-index", :context "wiki", :current_version 46,
       :publish_date "2010-05-18",
       :categories [{:id 1, :name "Clojure" :type :quick_category}
                    {:id 3, :name "SICP" :type :quick_category}]}
      {:id 9 :name "content-about" :context "wiki" :current_version 50
       :publish_date "2010-05-18"
       :categories [{:id 1 :name "Clojure" :type :quick_category}]}
      {:id 10 :name "content-tech-about" :context "wiki" :current_version 19
       :publish_date nil :categories []}])

(deftest test-dequalify-joined-map
  (are [x y z] (= (dequalify-joined-map x y) z)
       :quick_page
       {:quick_page_id 7, :quick_page_name "site-content",
        :quick_page_context "wiki", :quick_page_current_version 8,
        :quick_page_publish_date nil, :quick_category_id nil,
        :quick_category_name nil}
       {:id 7 :name "site-content" :context "wiki" :current_version 8
        :publish_date nil}

       :quick_category
       {:quick_page_id 7, :quick_page_name "site-content",
        :quick_page_context "wiki", :quick_page_current_version 8,
        :quick_page_publish_date nil, :quick_category_id 10,
        :quick_category_name "Some"}
       {:id 10 :name "Some"}))

(deftest test-transform-join-results
  (is (= (transform-join-results :quick_page model-fixture sample-join-from)
         sample-join-to)))

