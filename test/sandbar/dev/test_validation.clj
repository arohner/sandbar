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
  (is (= (add-validation-error {} :a "a")
         {:_validation-errors {:a ["a"]}}))
  (is (= (add-validation-error {:a "a"} :a "a")
         {:a "a" :_validation-errors {:a ["a"]}} ))
  (is (= (add-validation-error {:a "a" :_validation-errors {:a ["a"]}} :a "b")
         {:a "a" :_validation-errors {:a ["a" "b"]}}))
  (is (= (add-validation-error {:a "a" :_validation-errors {:a ["a"]}} :b "b")
         {:a "a" :_validation-errors {:a ["a"] :b ["b"]}})))

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
        (is (= (non-empty-string {:a ""} :a {:a "Username"})
               {:a "" :_validation-errors
                {:a ["The Username field cannot be blank?"]}})))
     (t "when nil instead of string"
        (is (= (non-empty-string {:a nil} :a "error")
               {:a nil :_validation-errors {:a ["error"]}})))
     (t "when value is non-empty string"
        (is (= (non-empty-string {:a "a"} :a "error")
               {:a "a"})))))



