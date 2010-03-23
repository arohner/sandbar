(ns sandbar.example.ideadb.test_user_module
  (:use [clojure.test]
        [sandbar.example.ideadb.user_module]
        [sandbar.library]
        [sandbar.test :only (t)]))

(defn test-request [params]
  {:session {:id "x"} :params params})

(def empty-idea {:status nil, :name nil, :user_id "t",
                 :date_entered (date-string), :customer_need nil,
                 :idea_type nil, :type :idea, :business_unit nil,
                 :originator nil, :description nil, :category nil})

(deftest test-create-idea-from-params
  (binding [session (atom {:x {:current-user {:name "t"}}})]
    (t "create idea from params"
       (t "when params are empty"
          (is (= (create-idea-from-params (test-request {}))
                 empty-idea)))
       (t "when params have a date_entered"
          (is (= (create-idea-from-params (test-request
                                           {:date_entered "2010-01-01"}))
                 (assoc empty-idea :date_entered "2010-01-01")))))))
