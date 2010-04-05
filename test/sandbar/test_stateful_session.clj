;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.test_stateful_session
  (:use (clojure test)
        (sandbar stateful-session)
        (sandbar.dev [test :only (t)])))

(deftest test-session-put!
  (t "put in session"
     (t "one element"
        (binding [*sandbar-session* (atom {})]
          (is (= (-> (session-put! :t "t")
                     :t)
                 "t"))))))

(deftest test-stateful-session
  (t "stateful session"
     (t "input empty, session in response"
        (is (= ((wrap-stateful-session* (fn [r] {:session {:a "a"}})) {})
               {:session {:a "a"}})))
     (t "input empty, use session-put!"
        (is (= ((wrap-stateful-session* (fn [r] (do (session-put! :a "a")
                                                   {}))) {})
               {:session {:sandbar-session {:a "a"}}})))
     (t "input contains values, use session-put! and return empty session"
        (is (= ((wrap-stateful-session*
                 (fn [r] (do (session-put! :a "a")
                             {})))
                {:session {:b "b"}})
               {:session {:sandbar-session {:a "a"} :b "b"}})))
     (t "input contains values, use session-put! and return empty session"
        (is (= ((wrap-stateful-session*
                 (fn [r] (do (session-put! :a "a")
                             {:session {}})))
                {:session {:b "b"}})
               {:session {:sandbar-session {:a "a"}}})))
     (t "input contains values, use session-put! and return empty session"
        (is (= ((wrap-stateful-session*
                 (fn [r] (do (session-put! :a "a")
                             {:session nil})))
                {:session {:b "b"}})
               {:session {:sandbar-session {:a "a"}}})))
     (t "input contains values, use sesion-put! and return nil session"
        (is (= ((wrap-stateful-session*
                 (fn [r] (do (session-put! :a "a")
                             {:session nil})))
                {:session {:b "b"}})
               {:session {:sandbar-session {:a "a"}}})))
     (t "input contains values, session in response replaces values"
        (is (= ((wrap-stateful-session*
                 (fn [r] {:session {:a "a"}}))
                {:session {:b "b"}})
               {:session {:a "a"}})))
     (t "input empty, use session-put! and return session"
        (is (= ((wrap-stateful-session*
                 (fn [r] (do (session-put! :a "a")
                             {:session {:b "b"}})))
                {})
               {:session {:sandbar-session {:a "a"} :b "b"}})))
     (t "input contains values, use session-put! and return session"
        (is (= ((wrap-stateful-session*
                 (fn [r] (do (session-put! :a "a")
                             {:session {:b "b"}})))
                {:session {:c "c"}})
               {:session {:sandbar-session {:a "a"} :b "b"}})))
     (t "input contains values, return empty DOES NOT remove values"
        (is (= ((wrap-stateful-session* (fn [r] {}))
                {:session {:a "a"}})
               {:session {:a "a"}})))
     (t "input contains values, return session nil removes values"
        (is (= ((wrap-stateful-session* (fn [r] {:session nil}))
                {:session {:a "a"}})
               {})))
     (t "input contains values, session-delete-key! does not remove them"
        (is (= ((wrap-stateful-session*
                 (fn [r] (do (session-delete-key! :a)
                             {})))
                {:session {:a "a"}})
               {:session {:a "a"}})))
     (t "input contains values, use session-delete-key! to remove them"
        (is (= ((wrap-stateful-session*
                 (fn [r] (do (session-delete-key! :a)
                             {})))
                {:session {:sandbar-session {:a "a"} :a "a"}})
               {:session {:a "a"}})))
     (t "response values DO NOT override existing session values"
        (is (= ((wrap-stateful-session*
                 (fn [r] (do (session-put! :a "b")
                             {:session {:a "c"}})))
                {:session {:a "a"}})
               {:session {:sandbar-session {:a "b"} :a "c"}})))))


