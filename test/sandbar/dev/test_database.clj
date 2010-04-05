; Copyright (c) Brenton Ashworth. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file COPYING at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns sandbar.dev.test_database
  (:use [clojure.test]
        [sandbar.dev.database]))

(deftest test-map-values
  (is (= (map-values #(if (nil? %) 42 %) {:name "John" :age 27 :height nil}) 
         {:name "John" :age 27 :height 42})))

; test split-criteria
(def split-criteria (ns-resolve 'sandbar.dev.database
                                'split-criteria)) 	
(deftest test-split-criteria
  (is (= (split-criteria {:id 1}) (list [:id] 1)))
  (is (= (split-criteria {:id 1 :name "John"}) (list [:id :name] 1 "John"))))

; test create-comparison
(def create-comparison (ns-resolve 'sandbar.dev.database
                                   'create-comparison))
(deftest test-create-comparison
  (is (= (create-comparison :id 1) "id = ?"))
  (is (= (create-comparison :name "John") "name = ?"))
  (is (= (create-comparison :name nil) "name IS NULL"))
  (is (= (create-comparison :name "Joh*") "name like ?")))

; test create-where-str
(def create-where-str (ns-resolve 'sandbar.dev.database
                                  'create-where-str))
(deftest test-create-where-str
  (is (= (create-where-str [:id] [1]) "id = ?"))
  (is (= (create-where-str [:id :name] [1 "John"]) "id = ? and name = ?"))
  (is (= (create-where-str [:id :name :desc :cost] [1 "John" "Something*" nil]) 
         "id = ? and name = ? and desc like ? and cost IS NULL")))
	
; test replace-wildcard
(def replace-wildcard (ns-resolve 'sandbar.dev.database
                                  'replace-wildcard))
(deftest test-replace-wildcard
  (is (= (replace-wildcard "Something*") "Something%")))
