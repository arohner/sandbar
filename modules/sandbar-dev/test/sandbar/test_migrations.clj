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

(defrecord MockDatabase [state] KeyValueStore
  (put-value
   [this key val]
   (swap! state assoc key val))
  (get-value
   [this key]
   (get @state key)))

(deftest test-run-all-migrations-after
  (let [state (atom {})
        result (run-all-migrations-after (MockDatabase. state)
                                         mig-ns
                                         20100515000)]
    (is (= result
           ["20100515001"]))
    (is (= (:database-version @state) "20100515001"))))

(deftest test-rollback-last-migration
  (let [state (atom {:database-version "20100515001"})
        result (rollback-last-migration (MockDatabase. state)
                                        mig-ns
                                        20100515001)]
    (is (= result
           ["20100515001"]))
    (is (= (:database-version @state) "20100515000")))
  (let [state (atom {:database-version "20100516001"})
        result (rollback-last-migration (MockDatabase. state)
                                        mig-ns
                                        20100516001)]
    (is (= result
           []))
    (is (= (:database-version @state) "20100516001"))))

(deftest test-migrate
  (let [state (atom {:database-version "20100515000"})
        result (migrate (MockDatabase. state)
                        mig-ns)]
    (is (= result
           ["20100515001"]))
    (is (= (:database-version @state) "20100515001"))))

(deftest test-rollback
  (let [state (atom {:database-version "20100515001"})
        result (rollback (MockDatabase. state) mig-ns)]
    (is (= result
           ["20100515001"]))
    (is (= (:database-version @state) "20100515000")))
  (let [state (atom {:database-version "20100515000"})
        result (rollback (MockDatabase. state) mig-ns)]
    (is (= result
           nil))
    (is (= (:database-version @state) "20100515000"))))
