;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.test-migrations
  (:use (clojure test)
        (sandbar database migrations)))

(def mig-ns (str *ns*))

(defn migration-20100515000 []
  {:forward (fn [db] true)})

(defn migration-20100515001 []
  {:forward (fn [db] true)
   :back (fn [db] true)})

(defn migration-20100516000 []
  {:forward (fn [db] false)
   :back (fn [db] false)})

(defn migration-20100516001 []
  {:forward (fn [db] (throw Exception "Test"))
   :back (fn [db] false)})

(defn migration-20100516002 []
  {:forward (fn [db] true)
   :back (fn [db] true)})

(deftest test-str->int
  (is (= (str->int "20100515001") 20100515001))
  (is (= (str->int "1") 1)))

(deftest test-find-all-migrations
  (is (= (find-all-migrations mig-ns)
         ["20100515000" "20100515001" "20100516000" "20100516001"
          "20100516002"])))

(deftest test-run-migration-ids
  (let [run-migration-ids (partial run-migration-ids {} mig-ns)]
    (are [x y] (= (run-migration-ids x) y)
         ["20100515000"] ["20100515000"]
         ["20100515000" "20100515001"] ["20100515000" "20100515001"]
         ["20100515001" "20100515000"] ["20100515000" "20100515001"]
         ["20100516000"] []
         ["20100515001" "20100516001"] ["20100515001"]
         ["20100515001" "20100516001" "20100516002"] ["20100515001"])))

(defn mock-database [v]
  (let [state (atom {:version v})]
    [(fn [db]
       (:version @state))
     (fn [db version]
       (swap! state assoc :version version))]))

(defmacro with-mock-database [v & body]
  `(let [database# (mock-database ~v)]
     (binding [~'get-version (first database#)
               ~'set-version (last database#)]
       ~@body)))

(deftest test-run-all-migrations-after
  (with-mock-database nil
    (let [result (run-all-migrations-after {}
                                           mig-ns
                                           20100515000)]
      (is (= result
             ["20100515001"]))
      (is (= (get-version {}) "20100515001")))))

(deftest test-rollback-last-migration
  (with-mock-database "20100515001"
    (let [result (rollback-last-migration {}
                                          mig-ns
                                          20100515001)]
     (is (= result
            ["20100515001"]))
     (is (= (get-version {}) "20100515000"))))
  (with-mock-database "20100516001"
    (let [result (rollback-last-migration {}
                                          mig-ns
                                          20100516001)]
      (is (= result
             []))
      (is (= (get-version {}) "20100516001")))))

(deftest test-migrate
  (with-mock-database "20100515000"
    (let [result (migrate {}
                          mig-ns)]
      (is (= result
             ["20100515001"]))
      (is (= (get-version {}) "20100515001")))))

(deftest test-rollback
  (with-mock-database "20100515001"
    (let [result (rollback {} mig-ns)]
      (is (= result
             ["20100515001"]))
      (is (= (get-version {}) "20100515000"))))
  (with-mock-database "20100515000"
    (let [result (rollback {} mig-ns)]
      (is (= result
             nil))
      (is (= (get-version {}) "20100515000")))))
