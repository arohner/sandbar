;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns example.ideadb.test-ideas
  (:use (clojure test)
        (sandbar auth util [test :only (t)])
        (example.ideadb ideas)))

(defn test-request [params]
  {:session {:id "x"} :params params})

(def empty-idea {:status nil, :name nil, :user_id "t",
                 :date_entered (date-string), :customer_need nil,
                 :idea_type nil, :type :idea, :business_unit nil,
                 :originator nil, :description nil, :category nil})

(deftest test-create-idea-from-params
  (binding [*sandbar-current-user* {:name "t"}]
    (t "create idea from params"
       (t "when params are empty"
          (is (= (create-idea-from-params {})
                 empty-idea)))
       (t "when params have a name"
          (is (= (create-idea-from-params {"name" "idea name"})
                 (assoc empty-idea :name "idea name"))))
       (t "when params have a date_entered"
          (is (= (create-idea-from-params {"date_entered" "2010-01-01"})
                 (assoc empty-idea :date_entered "2010-01-01")))))))
