;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.dev.test-forms
  (:use (clojure test)
        (sandbar core stateful-session)
        (sandbar.dev forms util
                     [test :only (t)])))

(deftest test-get-yes-no-fields
  (t "get yes/no fields"
     (t "when the field is selected"
        (is (= (get-yes-no-fields {}
                                  {:a "checkbox-true"}
                                  #{:a})
               {:a "Y"})))
     (t "when the field is not selected"
        (is (= (get-yes-no-fields {}
                                  {:a "checkbox-true"}
                                  #{:b})
               {:b "N"})))
     (t "when one field is selected and one is not"
        (is (= (get-yes-no-fields {}
                                  {:a "checkbox-true"}
                                  #{:a :b})
               {:a "Y" :b "N"})))))

(deftest test-get-multi-checkbox
  (t "get multi checkbox"
     (t "when one item is seleted"
        (is (= (get-multi-checkbox {}
                                   {"a" "a"}
                                   :a)
               {:a ["a"]})))
     (t "when two items are seleted"
        (is (= (get-multi-checkbox {}
                                   {"a" ["a" "b"]}
                                   :a)
               {:a ["a" "b"]})))
     (t "when nothing is seleted"
        (is (= (get-multi-checkbox {}
                                   {}
                                   :a)
               {:a []})))))

(defn create-test-multi-checkbox [fname coll]
  [:div [:span {} ""]
   (wrap-checkboxes-in-group
   (map
    #(let [value (first %)
           on-off (last %)
           attrs {:type "checkbox" :name fname :value value}
           attrs (if (= on-off :-)
                attrs
                (assoc attrs :checked "true"))]
       [:input attrs value])
    (partition 2 coll)))])

(deftest test-set-form-field-value-for-multi-checkbox
  (t "set multi checkbox field value"
     (t "when one of two checkboxs are selected"
        (is (= (set-form-field-value
                {:form-data {:test ["a"]}} 
                (create-test-multi-checkbox :test ["a" :- "b" :-]))
               (create-test-multi-checkbox :test ["a" :+ "b" :-]))))
     (t "when two of two checkboxs are selected"
        (is (= (set-form-field-value
                {:form-data {:test ["a" "b"]}} 
                (create-test-multi-checkbox :test ["a" :- "b" :-]))
               (create-test-multi-checkbox :test ["a" :+ "b" :+]))))))

(deftest test-index-by
  (t "index by"
     (t "when you have two distinct maps in a collection"
        (is (= (index-by :a [{:a "1" :b "A"}
                             {:a "2" :b "B"}])
               {"1" {:a "1" :b "A"}
                "2" {:a "2" :b "B"}})))))

(defn test-form-opt-label [field-name]
  [:div {:class "field-label"} field-name ""])

(defn test-form-req-label [field-name]
  [:div {:class "field-label"} field-name
   [:span {:class "required"} [:big " *"]]])

(defn form-textfield-fixture [field-name value]
  [:div [:input {:size 35 :type "Text" :name field-name :value value}]])

(defn form-hidden-fixture [field-name value]
  [:input {:type "hidden" :name field-name :value value}])

(defn form-password-fixture [field-name value]
  [:div [:input {:size 35 :type "Password" :name field-name :value value}]])

(deftest test-form-textfield
  (t "create a form textfield"
     (t "with only a title and name"
        (is (= (form-textfield "f1" :name)
               {:name [(test-form-opt-label "f1")
                       (last (form-textfield-fixture "name" ""))]})))
     (t "with a title, name and required"
        (is (= (form-textfield "f1" :name :required)
               {:name [(test-form-req-label "f1")
                       (last (form-textfield-fixture "name" ""))]})))
     (t "with a title, name and options"
        (is (= (form-textfield "f1" :name {:size 35})
               {:name [(test-form-opt-label "f1")
                       (last (form-textfield-fixture "name" ""))]})))
     (t "with properties and a name only"
        (is (= (form-textfield {:name "Name"} :name)
               {:name [(test-form-opt-label "Name")
                       (last (form-textfield-fixture "name" ""))]})))
     (t "with properties, name and options"
        (is (= (form-textfield {:name "Name"} :name {:size 35})
               {:name [(test-form-opt-label "Name")
                       (last (form-textfield-fixture "name" ""))]})))
     (t "with title, name, options and required"
        (is (= (form-textfield "f1" :name {:size 35} :required)
               {:name [(test-form-req-label "f1")
                       (last (form-textfield-fixture "name" ""))]})))
     (t "with properties, name, options and required"
        (is (= (form-textfield {:name "Name"} :name {:size 35} :required)
               {:name [(test-form-req-label "Name")
                       (last (form-textfield-fixture "name" ""))]})))))

(deftest test-form-password
  (t "create a form password"
     (t "with only a title and name"
        (is (= (form-password "f1" :name)
               {:name [(test-form-opt-label "f1")
                       (last (form-password-fixture "name" ""))]})))
     (t "with a title, name and required"
        (is (= (form-password "f1" :name :required)
               {:name [(test-form-req-label "f1")
                       (last (form-password-fixture "name" ""))]})))
     (t "with a title, name and options"
        (is (= (form-password "f1" :name {:size 35})
               {:name [(test-form-opt-label "f1")
                       (last (form-password-fixture "name" ""))]})))
     (t "with properties and a name only"
        (is (= (form-password {:name "Name"} :name)
               {:name [(test-form-opt-label "Name")
                       (last (form-password-fixture "name" ""))]})))
     (t "with properties, name and options"
        (is (= (form-password {:name "Name"} :name {:size 35})
               {:name [(test-form-opt-label "Name")
                       (last (form-password-fixture "name" ""))]})))
     (t "with title, name, options and required"
        (is (= (form-password "f1" :name {:size 35} :required)
               {:name [(test-form-req-label "f1")
                       (last (form-password-fixture "name" ""))]})))
     (t "with properties, name, options and required"
        (is (= (form-password {:name "Name"} :name {:size 35} :required)
               {:name [(test-form-req-label "Name")
                       (last (form-password-fixture "name" ""))]})))))

(defn test-form-textarea [field-name value]
  [:div (if (empty? value)
          [:textarea {:name field-name}]
          [:textarea {:name field-name} value])])

(defn form-checkbox-fixture [field-name value]
  [:input {:type "checkbox" :name field-name :value "checkbox-true"}])

(defn form-checkbox-label-fixture [title]
  [:span {:class "field-label"} title])

(deftest test-form-checkbox
  (t "create a checkbox"
     (t "with a title and a name"
        (is (= (form-checkbox "f1" :name)
               {:name [(form-checkbox-label-fixture "f1")
                       (form-checkbox-fixture "name" false)]})))
     (t "with properties and a name"
        (is (= (form-checkbox {:name "Name"} :name)
               {:name [(form-checkbox-label-fixture "Name")
                       (form-checkbox-fixture "name" false)]})))
     (t "with a title, name and empty options"
        (is (= (form-checkbox "f1" :name {})
               {:name [(form-checkbox-label-fixture "f1")
                       (form-checkbox-fixture "name" false)]})))
     (t "with a title, name and empty options"
        (is (= (form-checkbox {:name "Name"} :name {})
               {:name [(form-checkbox-label-fixture "Name")
                       (form-checkbox-fixture "name" false)]})))))

(defn test-form-multi-checkbox [field-name value]
  [:div [:input {:type "Text", :name field-name, :value value}]])

(defn test-form-table [layout & cell-values]
  (vec
   (conj [:div] (apply layout-table layout cell-values))))

(deftest test-create-form-field-cell
  (t "create form field cell"
     (t "containing a text field"
        (is (= (create-form-field-cell
                {}
                (form-textfield "f1" :name))
               [:div
                (test-form-opt-label "f1")
                (form-textfield-fixture "name" "")])))
     (t "containing a text area"
        (is (= (create-form-field-cell
                {}
                (form-textarea "f1" :name {}))
               [:div
                (test-form-opt-label "f1")
                (test-form-textarea "name" "")])))
     (t "containing a checkbox"
        (is (= (create-form-field-cell
                {}
                (form-checkbox "f1" :name))
               [:div
                [:div "&nbsp;"]
                [:div
                 (form-checkbox-fixture "name" false)
                 [:span {:class "field-label"} "f1"]]])))
     (t "containing a text checkbox group"
        (is (= (create-form-field-cell
                {}
                (form-multi-checkbox {}
                                     :name [{:id 1 :value "a"}
                                            {:id 2 :value "b"}]
                                     :value))
               [:div
                [:div]
                [:div
                 [:div
                  [:span {:class "group-title"} "name"]
                  [:div {:class "group"}
                   [[:div {:class "group-checkbox"}
                     [:input {:type "checkbox", :name :name, :value "a"} "a"]]
                    [:div {:class "group-checkbox"}
                     [:input {:type "checkbox", :name :name, :value "b"} "b"]]]]]]])))))

(deftest test-form-layout
  (binding [*sandbar-session* (atom {})]
    (t "create form layout"
       (t "with a single required text field and no initial state"
          (is (= (form-layout-grid
                  :test
                  [(form-textfield "f1" :name :required)]
                  {})
                 (test-form-table [1]
                                  [[:div
                                    (test-form-req-label "f1")
                                    (form-textfield-fixture "name" "")]]))))
       (t "with a single optional text field and no initial state"
          (is (= (form-layout-grid
                  :test
                  [(form-textfield "f1" :name)]
                  {})
                 (test-form-table [1]
                                  [[:div
                                    (test-form-opt-label "f1")
                                    (form-textfield-fixture "name" "")]]))))
       (t "with a single optional text field and an initial state"
          (is (= (form-layout-grid
                  one-column-layout
                  :test
                  [(form-textfield "f1" :name)]
                  {}
                  {:name "n"})
                 (test-form-table [1]
                                  [[:div
                                    (test-form-opt-label "f1")
                                    (form-textfield-fixture "name" "n")]]))))
       (t "with two optional text fields and no initial state"
          (is (= (form-layout-grid
                  :test
                  [(form-textfield "f1" :name)
                   (form-textfield "f2" :age)]
                  {})
                 (test-form-table [1 1]
                                  [[:div
                                    (test-form-opt-label "f1")
                                    (form-textfield-fixture "name" "")]]
                                  [[:div
                                    (test-form-opt-label "f2")
                                    (form-textfield-fixture "age" "")]]))))
       (t "explicitly using one column layout - layout vec has 2 of 4 values"
          (is (= (form-layout-grid
                  one-column-layout
                  :test
                  [(form-textfield "f1" :name)
                   (form-textfield "f2" :age)]
                  {})
                 (test-form-table [1 1]
                                  [[:div
                                    (test-form-opt-label "f1")
                                    (form-textfield-fixture "name" "")]]
                                  [[:div
                                    (test-form-opt-label "f2")
                                    (form-textfield-fixture "age" "")]]))))
       (t "using a two column layout"
          (is (= (form-layout-grid
                  [2]
                  :test
                  [(form-textfield "f1" :name)
                   (form-textfield "f2" :age)]
                  {})
                 (test-form-table [2]
                                  [[:div
                                    (test-form-opt-label "f1")
                                    (form-textfield-fixture "name" "")]]
                                  [[:div
                                    (test-form-opt-label "f2")
                                    (form-textfield-fixture "age" "")]]))))
       (t "using a mix of one and two columns"
          (is (= (form-layout-grid
                  [1 2]
                  :test
                  [(form-textfield "f1" :name)
                   (form-textfield "f2" :age)
                   (form-textfield "f3" :title)]
                  {})
                 (test-form-table [1 2]
                                  [[:div
                                    (test-form-opt-label "f1")
                                    (form-textfield-fixture "name" "")]]
                                  [[:div
                                    (test-form-opt-label "f2")
                                    (form-textfield-fixture "age" "")]]
                                  [[:div
                                    (test-form-opt-label "f3")
                                    (form-textfield-fixture "title" "")]]))))
       (t "with one hidden field"
          (is (= (form-layout-grid
                  :test
                  [(form-hidden :title)
                   (form-textfield "f1" :name)
                   (form-textfield "f2" :age)]
                  {})
                 [:div
                  (layout-table []
                                [[:div
                                  (test-form-opt-label "f1")
                                  (form-textfield-fixture "name" "")]]
                                [[:div
                                  (test-form-opt-label "f2")
                                  (form-textfield-fixture "age" "")]])
                  (form-hidden-fixture "title" "")])))
       (t "with two hidden fields"
          (is (= (form-layout-grid
                  :test
                  [(form-hidden :title)
                   (form-hidden :id)
                   (form-textfield "f1" :name)
                   (form-textfield "f2" :age)]
                  {})
                 [:div
                  (layout-table []
                                [[:div
                                  (test-form-opt-label "f1")
                                  (form-textfield-fixture "name" "")]]
                                [[:div
                                  (test-form-opt-label "f2")
                                  (form-textfield-fixture "age" "")]])
                  (form-hidden-fixture "title" "")
                  (form-hidden-fixture "id" "")]))))))

(def td-std-opts {:valign "top"})

(deftest test-layout-table
  (t "create a layout table"
     (t "with one cell"
        (is (= (layout-table [1] "A")
               [:table [:tr [:td td-std-opts "A"]]])))
     (t "with one cell on each row"
        (is (= (layout-table [1 1] "A" "B")
               [:table
                [:tr [:td td-std-opts "A"]]
                [:tr [:td td-std-opts "B"]]])))
     (t "with two things going into one row"
        (is (= (layout-table [1] ["A" "B"])
               [:table
                [:tr [:td td-std-opts "A" "B"]]])))
     (t "with a vector of two things going into one cell"
        (is (= (layout-table [1] [["A" "B"]])
               [:table
                [:tr [:td td-std-opts ["A" "B"]]]])))
     (t "with two things going into one cell and then two things in two cells"
        (is (= (layout-table [1 2] ["A" "B"] "C" "D")
               [:table
                [:tr [:td {:colspan 2 :valign "top"} "A" "B"]]
                [:tr [:td td-std-opts "C"] [:td td-std-opts "D"]]])))
     (t "with two cells in one row"
        (is (= (layout-table [2] "A" "B")
               [:table
                [:tr [:td td-std-opts "A"] [:td td-std-opts "B"]]])))
     (t "with two cells on each of two rows"
        (is (= (layout-table [2 2] "A" "B" "C" "D")
               [:table
                [:tr [:td td-std-opts "A"] [:td td-std-opts "B"]]
                [:tr [:td td-std-opts "C"] [:td td-std-opts "D"]]])))
     (t "with two cells on the first row and one cell on the second"
        (is (= (layout-table [2 1] "A" "B" "C")
               [:table
                [:tr [:td td-std-opts "A"] [:td td-std-opts "B"]]
                [:tr [:td {:colspan 2 :valign "top"} "C"]]])))
     (t "with one cell on the first row and two cells on the second"
        (is (= (layout-table [1 2] "A" "B" "C")
               [:table
                [:tr [:td {:colspan 2 :valign "top"} "A"]]
                [:tr [:td td-std-opts "B"] [:td td-std-opts "C"]]])))
     (t "with a pyramid layout"
        (is (= (layout-table [1 2 3 2 1] "A" "B" "C" "D" "E" "F" "G" "H" "I")
               [:table
                [:tr [:td {:colspan 3 :valign "top"} "A"]]
                [:tr [:td td-std-opts "B"] [:td {:colspan 2 :valign "top"} "C"]]
                [:tr
                 [:td td-std-opts "D"]
                 [:td td-std-opts "E"]
                 [:td td-std-opts "F"]]
                [:tr [:td td-std-opts "G"] [:td {:colspan 2 :valign "top"} "H"]]
                [:tr [:td {:colspan 3 :valign "top"} "I"]]])))
     (t "will a nil cell value"
        (is (= (layout-table [] "A" nil)
               [:table
                [:tr [:td {:valign "top"} "A"]]])))))
