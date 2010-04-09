;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.dev.test-validation
  (:use (clojure test)
        (sandbar.dev validation
                     [test :only (t)])))

(deftest test-add-validation-errors
  (t "add validation error"
     (t "when errors are empt"
        (is (= (add-validation-error {} :a "a")
               {:_validation-errors {:a ["a"]}})))
     (t "when no key is passed, error goes in :form key"
        (is (= (add-validation-error {} "a")
               {:_validation-errors {:form ["a"]}})))
     (t "when error key exists in form data"
        (is (= (add-validation-error {:a "a"} :a "a")
               {:a "a" :_validation-errors {:a ["a"]}} )))
     (t "when validation errors exist and putting new value in same key"
        (is (= (add-validation-error {:a "a" :_validation-errors {:a ["a"]}}
                                     :a "b")
               {:a "a" :_validation-errors {:a ["a" "b"]}})))
     (t "when validation errors exist and adding new error"
        (is (= (add-validation-error {:a "a" :_validation-errors {:a ["a"]}}
                                     :b "b")
               {:a "a" :_validation-errors {:a ["a"] :b ["b"]}})))))

(defn example-validator
  "A validator takes a map as an argument and then returns the map with a
   key named :_validation-errors that contains a map or error messages."
  [m]
  (if (= (:a m) "a")
    m
    (add-validation-error m :a ":a does not contain the letter a")))

(deftest test-if-valid
  (t "testing if-valid"
     (t "with built-in functions and validation is true"
          (is (= (if-valid identity {:a "a"} identity list)
                 {:a "a"})))
     (t "with custom functions"
        (let [success (fn [a] (:a a))
              failure (fn [a b] (first (:a b)))]
          (t "when validation is true"
             (is (= (if-valid example-validator {:a "a"}
                              success
                              failure)
                    "a")))
          (t "when validation is false"
             (is (= (if-valid example-validator {:a "b"}
                              success
                              failure)
                    ":a does not contain the letter a")))))))

(deftest test-non-empty-string
  (t "test non-empty-string"
     (t "when string is empty"
        (is (= (non-empty-string {:a ""} :a "error")
               {:a "" :_validation-errors {:a ["error"]}}))
        (t "with map message"
           (is (= (non-empty-string {:a ""} :a {:a "Username"})
                  {:a "" :_validation-errors
                   {:a ["Username cannot be blank!"]}})))
        (t "with empty message"
           (is (= (non-empty-string {:a ""} :a)
                  {:a "" :_validation-errors
                   {:a ["a cannot be blank!"]}})))
        (t "with custom error message"
           (is (= (non-empty-string {:a ""} :a {:a-validation-error "x"})
                  {:a "" :_validation-errors
                   {:a ["x"]}}))))
     (t "when nil instead of string"
        (is (= (non-empty-string {:a nil} :a "error")
               {:a nil :_validation-errors {:a ["error"]}})))
     (t "when value is non-empty string"
        (is (= (non-empty-string {:a "a"} :a "error")
               {:a "a"})))))

(deftest test-build-validator
  (t "build validator"
     (t "out of one validator fn and validation passes"
        (is (= ((build-validator (non-empty-string :a)) {:a "a"})
               {:a "a"})))
     (t "out of one validator fn and validation fails"
        (is (= ((build-validator (non-empty-string :a)) {:a ""})
               {:a ""
                :_validation-errors {:a ["a cannot be blank!"]}})))
     (t "out of two validators fns and validation passes"
        (is (= ((build-validator (non-empty-string :a)
                                 (non-empty-string :b)) {:a "a" :b "b"})
               {:a "a" :b "b"})))
     (t "out of two validators fns and validation fails for one"
        (is (= ((build-validator (non-empty-string :a)
                                 (non-empty-string :b)) {:a "a" :b ""})
               {:a "a" :b ""
                :_validation-errors {:b ["b cannot be blank!"]}})))
     (t "out of three validators with an ensure, all validations pass"
        (is (= ((build-validator (non-empty-string :a)
                                 (non-empty-string :b)
                                 :ensure
                                 (non-empty-string :c)) {:a "a" :b "b" :c "c"})
               {:a "a" :b "b" :c "c"})))
     (t "out of four validators with an ensure in the middle, all pass"
        (is (= ((build-validator (non-empty-string :a)
                                 (non-empty-string :b)
                                 :ensure
                                 (non-empty-string :c)
                                 (non-empty-string :d))
                {:a "a" :b "b" :c "c" :d "d"})
               {:a "a" :b "b" :c "c" :d "d"})))
     (t "error after :ensure is not detected if there is error before"
        (is (= ((build-validator (non-empty-string :a)
                                 (non-empty-string :b)
                                 :ensure
                                 (non-empty-string :c)) {:a "a" :b "" :c ""})
               {:a "a" :b "" :c ""
                :_validation-errors {:b ["b cannot be blank!"]}})))
     (t "error after :ensure is detected if no error before"
        (is (= ((build-validator (non-empty-string :a)
                                 (non-empty-string :b)
                                 :ensure
                                 (non-empty-string :c)) {:a "a" :b "b" :c ""})
               {:a "a" :b "b" :c ""
                :_validation-errors {:c ["c cannot be blank!"]}})))
     (t "thee levels of nesting"
        (is (= ((build-validator (non-empty-string :a)
                                 (non-empty-string :b)
                                 :ensure
                                 (non-empty-string :c)
                                 (non-empty-string :d)
                                 :ensure
                                 (non-empty-string :e)
                                 (non-empty-string :f))
                {:a "a" :b "b" :c "c" :d "d" :e "e" :f "f"})
               {:a "a" :b "b" :c "c" :d "d" :e "e" :f "f"})))
     (t "errors in first group hides other errors"
        (is (= ((build-validator (non-empty-string :a)
                                 (non-empty-string :b)
                                 :ensure
                                 (non-empty-string :c)
                                 (non-empty-string :d)
                                 :ensure
                                 (non-empty-string :e)
                                 (non-empty-string :f))
                {:a "a" :b "" :c "" :d "" :e "" :f ""})
               {:a "a" :b "" :c "" :d "" :e "" :f ""
                :_validation-errors {:b ["b cannot be blank!"]}})))
     (t "errors in second group hides third group's errors"
        (is (= ((build-validator (non-empty-string :a)
                                 (non-empty-string :b)
                                 :ensure
                                 (non-empty-string :c)
                                 (non-empty-string :d)
                                 :ensure
                                 (non-empty-string :e)
                                 (non-empty-string :f))
                {:a "a" :b "b" :c "" :d "d" :e "" :f ""})
               {:a "a" :b "b" :c "" :d "d" :e "" :f ""
                :_validation-errors {:c ["c cannot be blank!"]}})))
     (t "find errors in third group"
        (is (= ((build-validator (non-empty-string :a)
                                 (non-empty-string :b)
                                 :ensure
                                 (non-empty-string :c)
                                 (non-empty-string :d)
                                 :ensure
                                 (non-empty-string :e)
                                 (non-empty-string :f))
                {:a "a" :b "b" :c "c" :d "d" :e "" :f ""})
               {:a "a" :b "b" :c "c" :d "d" :e "" :f ""
                :_validation-errors {:e ["e cannot be blank!"]
                                     :f ["f cannot be blank!"]}})))))

