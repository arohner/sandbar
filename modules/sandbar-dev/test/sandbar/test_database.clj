;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.test-database
  (:use (clojure test)
        (sandbar test database migrations
                 [sample-database-migrations :only (create-key-value-table)])))

(deftest test-split-with
  (are [x y] (is (= (split-with #(not (= % :c)) x) y))
       [:a :b :c :d] [[:a :b] [:c :d]]))

(deftest test-map-values
  (is (= (map-values #(if (nil? %) 42 %) {:name "John" :age 27 :height nil}) 
         {:name "John" :age 27 :height 42})))

(deftest test-split-criteria
  (are [a _ result] (= (split-criteria a) result)
       {:id 1}              :=> [[:id] 1]
       {:id 1 :name "John"} :=> [[:id :name] 1 "John"]))

(deftest test-create-comparison
  (are [a b _ result] (= (create-comparison a b) result) 
       :id 1        :=> "id = ?"
       :name "John" :=> "name = ?"
       :name nil    :=> "name IS NULL"
       :name "Joh*" :=> "name like ?"))

(deftest test-create-where-str
  (are [a b _ result] (= (create-where-str a b) result)
       [:id] [1]                   :=> "id = ?"
       [:id :name] [1 "John"]      :=>"id = ? AND name = ?"
       [:id :name :desc :cost]
       [1 "John" "Something*" nil] :=>
       "id = ? AND name = ? AND desc like ? AND cost IS NULL"))

(deftest test-create-where-vec
  (are [x y] (= (create-where-vec x) y)
       [] nil
       [{:name "a"}] ["name = ?" "a"]
       [{:name "a" :age 7}] ["name = ? AND age = ?" "a" 7]
       [{:name "a"} {:name "b"}] ["(name = ?) OR (name = ?)" "a" "b"]
       [{:name "a" :age 7} {:name "b"}]
       ["(name = ? AND age = ?) OR (name = ?)" "a" 7 "b"]))

(deftest test-replace-wildcard
  (is (= (replace-wildcard "Something*") "Something%")))

(def fixture-model-many-to-many
     (model
      (category [:id :name])
      (page [:id :name :current_version]
            (many-to-many :category))))

(def fixture-model-one-to-many
     (model
      (version [:id :content])
      (page [:id :name :current_version]
            (one-to-many versions :version :page_id))))

(def fixture-model-one-and-many-to-many
     (model
      (category [:id :name])
      (version [:id :content])
      (page [:id :name :current_version]
            (many-to-many :category)
            (one-to-many :version))))

(def fixture-join-category
     {:type :many-to-many
      :relation :category
      :alias :categories
      :link :page_category
      :from :page_id
      :to :category_id})

(def fixture-join-version
     {:type :one-to-many
      :relation :version
      :alias :versions
      :link :page_id})

(def fixture-artist-album-model
     {:album {:attrs [:id :title]
              :joins #{{:type :many-to-many
                        :relation :artist
                        :alias :artists
                        :link :album_artist
                        :from :album_id
                        :to :artist_id}}}
      :artist {:attrs [:id :name]
               :alias :artists}})

(deftest test-model
  (t "test creating a model"
     (t "containing a many-to-many relationship"
        (is (= fixture-model-many-to-many
               {:category {:attrs [:id :name]
                            :alias :categories}
                 :page {:attrs [:id :name :current_version]
                        :joins #{fixture-join-category}}})))
     (t "containing a one-to-many relationship"
        (is (= fixture-model-one-to-many
               {:version {:attrs [:id :content]
                           :alias :versions}
                 :page {:attrs [:id :name :current_version]
                        :joins #{fixture-join-version}}})))
     (t "containing both one and many -to-many"
        (is (= fixture-model-one-and-many-to-many
               {:category {:attrs [:id :name]
                            :alias :categories}
                 :version {:attrs [:id :content]
                           :alias :versions}
                 :page {:attrs [:id :name :current_version]
                        :joins #{fixture-join-category
                                 fixture-join-version}}})))
     (t "using the relation macro"
        (are [x] (= x fixture-artist-album-model)

             (model
              (album [:id :title]
                     (many-to-many artists :artist
                                   => :album_artist :album_id :artist_id))
              (artist [:id :name]))

             (model
              (album [:id :title]
                     (many-to-many artists :artist => :album_artist))
              (artist [:id :name]))

             (model
              (album [:id :title]
                     (many-to-many artists :artist))
              (artist [:id :name]))

             (model
              (album [:id :title]
                     (many-to-many :artist => :album_artist))
              (artist [:id :name]))
             
             (model
              (album [:id :title]
                     (many-to-many :artist))
              (artist [:id :name])))
        (are [x] (= x {:version {:alias :versions}
                       :page {:attrs [:name]
                              :joins #{{:type :one-to-many
                                        :alias :versions
                                        :relation :version
                                        :link :page_id}}}})
             (model (page [:name]
                          (one-to-many versions :version :page_id)))
             
             (model (page [:name]
                          (one-to-many :version :page_id)))

             (model (page [:name]
                          (one-to-many :version)))))))

(defn attr-as [table col]
  (let [table (name table)
        col (name col)]
    (str table "." col " as " table "_" col)))

(defn comma-delim-str [coll]
  (apply str (interpose ", " coll)))

(defn attr-list [coll]
  (str " "
       (comma-delim-str
        (map #(let [[table attrs] %]
                (comma-delim-str
                 (reduce (fn [a b]
                           (conj a (attr-as table b)))
                         []
                         attrs)))
             coll))))

(def page-table [:page [:id :name :current_version]])
(def category-table [:category [:id :name]])
(def version-table [:version [:id :content]])

(deftest test-create-attr-list
  (is (= (create-attr-list nil :page) " *"))
  (is (= (create-attr-list {} :page) " *"))
  (are [model q j _ attrs] (= (create-attr-list model :page q j)
                                (attr-list attrs)))

  fixture-model-one-and-many-to-many
  nil
  {:page [:categories :versions]} :=> [page-table
                                       category-table
                                       version-table]
  
  
  fixture-model-many-to-many
  {:page [:name]}
  {:page [:categories]}           :=> [[:page [:name :id]]
                                       category-table]
  
  fixture-model-many-to-many
  {:page [:name] :category [:name]}
  {:page [:categories]}           :=> [[:page [:name :id]]
                                       [:category [:name :id]]])

(deftest test-parse-query
  (are [x _ y] (= (parse-query :page x) y)
       []                        :=> {}
       [[:id]]                   :=> {:attrs {:page [:id]}}
       [{:name "a"}]             :=> {:criteria [{:name "a"}]}
       [{:name "a" :age 7}]      :=> {:criteria [{:name "a" :age 7}]}
       [{:name "a"} {:name "b"}] :=> {:criteria [{:name "a"} {:name "b"}]}
       [[:id] {:name "a"}]       :=> {:attrs {:page [:id]}
                                      :criteria [{:name "a"}]}))

(defn select-from [attrs]
  (str "SELECT" attrs " FROM page"))

(def many-to-many-join-query
     (str (select-from
           (attr-list [page-table category-table]))
          " LEFT JOIN page_category ON page.id = page_category.page_id "
          "LEFT JOIN category ON page_category.category_id = category.id"))

(def one-to-many-join-query
     (str (select-from
           (attr-list [page-table version-table]))
          " LEFT JOIN version ON page.id = version.page_id"))

(def one-and-many-to-many-join-query
     (str (select-from
           (attr-list [page-table category-table version-table]))
          " LEFT JOIN page_category ON page.id = page_category.page_id "
          "LEFT JOIN category ON page_category.category_id = category.id "
          "LEFT JOIN version ON page.id = version.page_id"))

(deftest test-create-selects
  (are [model q _ result]
       (= (create-selects {:subprotocol "mysql"} model :page q)
          result)
       
       {} []                          :=> [[(select-from " *")]]
       nil []                         :=> [[(select-from " *")]]
       nil [{:id 1}]                  :=> [[(str (select-from " *")
                                                 " WHERE page.id = ?") 1]]
       nil [{:name "brent*"}]         :=> [[(str (select-from " *")
                                                 " WHERE page.name like ?")
                                            "brent%"]]
       fixture-model-many-to-many
       []                             :=> [[(select-from
                                             (attr-list [page-table]))]]
       fixture-model-many-to-many
       [:with :categories]            :=> [[many-to-many-join-query]]
       fixture-model-many-to-many
       [{:id 1}]                      :=> [[(str (select-from
                                                  (attr-list [page-table]))
                                                 " WHERE page.id = ?") 1]]
       fixture-model-many-to-many
       [{:id 1} :with :categories]    :=> [[(str many-to-many-join-query
                                                 " WHERE page.id = ?") 1]]
       fixture-model-many-to-many
       [{:id 1 :name "brenton"}
        :with :categories]            :=> [[(str many-to-many-join-query
                                                 " WHERE page.id = ?"
                                                 " AND page.name = ?")
                                            1 "brenton"]]
        fixture-model-one-to-many
        [:with :versions]             :=> [[one-to-many-join-query]]
        fixture-model-one-and-many-to-many
        [:with :categories :versions] :=> [[one-and-many-to-many-join-query]]))

(deftest test-dequalify-joined-map
  (are [x y z] (= (dequalify-joined-map fixture-model-many-to-many x y) z)
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
     [{:id 7 :name "one" :current_version 1 :categories []}
      {:id 8 :name "two" :current_version 2
       :categories [{:id 1 :name "Clojure"}
                    {:id 3 :name "SICP"}]}
      {:id 9 :name "three" :current_version 3
       :categories [{:id 1 :name "Clojure"}]}])

(def type-key :sandbar.database/type)
(def orig-key :sandbar.database/original)

(deftest test-transform-query-plan-results
  (let [t-q-p-r
        (fn [model res]
          (transform-query-plan-results {:subprotocol "mysql"}
                                        model
                                        :page
                                        [:with :categories]
                                        res))]
    (t "test transform query plan results"
       (let [result (t-q-p-r fixture-model-many-to-many
                             fixture-join-flat)
            first-result (first result)
            categories (-> result second :categories)
            first-cat (first categories)]
        (t "- is the entire structure correct"
           (is (= result fixture-join-nested)))
        (t "- is metadata correct on top level item"
           (is (= (meta first-result)
                  {type-key :page
                   orig-key first-result})))
        (t "- does metadata have original value"
           (is (= first-result
                  (-> first-result meta orig-key))))
        (t "- does a specific category contain the correct metadata"
           (is (= (meta first-cat)
                  {type-key :category
                   orig-key first-cat})))
        (t "- does metadata for category contain the original value"
           (is (= first-cat (-> first-cat meta orig-key)))))
      (t "with common prefixes"
         (is (= (t-q-p-r (model
                          (page_category [:id :name])
                          (page [:id :name]
                                (one-to-many categories
                                             :page_category :page_id)))
                         [[{:page_id 1 :page_name "one" :page_category_id 1
                            :page_category_name "c1"}]])
                [{:id 1 :name "one"
                  :categories [{:id 1 :name "c1"}]}]))))))

(deftest test-dismantle-record
  (let [result
        (dismantle-record
         fixture-model-many-to-many
         (with-meta
           {:id 1 :name "a" :current_version 2
            :categories [(with-meta {:id 1 :name "a"}
                                      {type-key :category
                                       orig-key
                                       {:id 1 :name "a"}})
                                    (with-meta {:id 2 :name "b"}
                                      {type-key :category
                                       orig-key
                                       {:id 2 :name "b"}})]}
           {type-key :page
            orig-key
            {:id 1 :name "a" :current_version 2}}))]
    (is (= result
           {:base-record {:id 1 :name "a" :current_version 2}
            :categories [{:id 1 :name "a"}
                         {:id 2 :name "b"}]}))
    (is (= (-> result :base-record meta)
           {type-key :page
            orig-key
            {:id 1 :name "a" :current_version 2}}))
    (is (= (-> result :categories first meta)
           {type-key :category
            orig-key
            {:id 1 :name "a"}}))))

(deftest test-dirty?
  (t "test dirty?"
     (let [record {:id 1 :name "a"}]
       (are [s x y] (t s (is (= (dirty? y) x)))
            "with no metadata"
            true record
            "with metadata but no original"
            true (with-meta record {type-key :a})
            "with metadata and different original"
            true (with-meta record {type-key :a
                                    orig-key
                                    {:id 1 :name "b"}})
            "with correct original"
            false (with-meta record {type-key :a
                                     orig-key record})))))

(deftest test-find-join-model
  (are [x y z m] (is (= (find-join-model x y z) m))
       fixture-model-many-to-many :page :categories
       fixture-join-category
       fixture-model-one-to-many :page :versions
       fixture-join-version
       fixture-model-one-and-many-to-many :page :categories
       fixture-join-category
       fixture-model-one-and-many-to-many :page :versions
       fixture-join-version))

;; The following tests require that you have a mysql database
;; available on localhost. Use the following commands in mysql to
;; create the database;

;; CREATE DATABASE sandbar_test_db;
;; GRANT ALL PRIVILEGES ON sandbar_test_db.* TO sandbar_user@localhost
;;   IDENTIFIED BY '123456789';

(def db {:connection {:classname "com.mysql.jdbc.Driver"
                      :subprotocol "mysql"
                      :subname "//localhost/sandbar_test_db"
                      :user "sandbar_user"
                      :password "123456789"}})


(def data-model
     (model
      (album [:id :title]
             (many-to-many :artist))
      (artist [:id :name]
              (many-to-many :album => :album_artist))))

(def ! (partial save-or-update db data-model))
(def $ (partial query db data-model))
(def $1 (partial query-1 db data-model))

(defn build-test-database []
  (do
    (try
     (create-key-value-table (partial db-do-commands db))
     (catch Exception _ false))
    (try
     (! :key_value {:key_name "database-version" :value "20100524000"})
     (migrate db "sandbar.sample-database-migrations")
     true
     (catch Exception _ false))))

(defn delete-all-test-data []
  (do
    (doseq [next ($ :album_artist)]
      (delete-record db next))
    (doseq [next ($ :album)]
      (delete-record db next))
    (doseq [next ($ :artist)]
      (delete-record db next))))

(defn default-test-data []
  (do
    (! :album [{:title "Magic Potion"}
               {:title "Thickfreakness"}
               {:title "Let's Dance"}])
    (! :artist [{:name "The Black Keys"}
                {:name "David Bowie"}])
    (! (conj-in ($1 :album {:title "Magic Potion"} :with :artists)
                [:artists]
                ($1 :artist {:name "The Black Keys"})))))

(defn ensure-test-database
  "Ensure that the database exists and contains the tables that you will
   need."
  []
  (do
    (try
     (let [v ($ :key_value {:key_name "database-version"})]
       (if (not (seq v))
         (build-test-database)))
     (catch Exception _ (build-test-database)))
    (try
     (migrate db "sandbar.sample-database-migrations")
     (catch Exception _ false))))

(defmacro with-test-database [data-set & body]
  `(do
     (if (~'ensure-test-database)
       (do
         (~data-set)
         (try
          ~@body
          (catch Exception ~'_ false)
          (finally (~'delete-all-test-data))))
       (println "!!WARNING!! Not running tests against mysql."))))

(deftest test-query
  (binding [*debug* false]
    (with-test-database default-test-data
      (are [q f expected] (= (f q) expected)
           
           (query db ["select * from album"]) count 3
           
           (query db ["select * from album where title = \"Magic Potion\""])
           #(:title (first %))
           "Magic Potion"

           (query-1 db ["select * from album where title = \"Magic Potion\""])
           :title
           "Magic Potion"

           (query db :album) count 3

           (query db :artist)
           #(set (map :name %))
           #{"David Bowie" "The Black Keys"}

           (query db :artist [:name])
           #(set (map :name %))
           #{"David Bowie" "The Black Keys"}

           ($ :artist [:name])
           #(set (map :name %))
           #{"David Bowie" "The Black Keys"}

           ($ :album) count 3

           ($1 :artist [:name] {:name "David Bowie"})
           :name
           "David Bowie"

           ($ :album {:title "Mag*"} {:title "Th*"})
           #(set (map :title %))
           #{"Magic Potion" "Thickfreakness"}
           
           ;; TODO - Simplify selecting items like this
           ($ :album :with :artists)
           #(:name (first (:artists (first (filter (fn [x] (= (:title x) "Magic Potion")) %)))))
           "The Black Keys"

           ($1 :album {:title "Magic Potion"} :with :artists)
           #(:name (first (:artists %)))
           "The Black Keys"

           ($ :artist :with :albums)
           #(:title (first (:albums (first (filter (fn [x] (= (:name x) "The Black Keys")) %)))))
           "Magic Potion"
           

           )
      
     (let [result ($1 :album {:title "Magic Potion"})]
       (is (= (:title result) "Magic Potion"))
       (is (= (-> result meta type-key) :album))))))

(comment
  
  ;; Think about making a way to allow this to work. It will need to
  ;; get the columns from the database.
  (def data-model
       (model
        (album [many-to-many :artist])
        (artist)))
  )

