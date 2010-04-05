;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.dev.test_userui
  (:use [clojure.test]
        (sandbar [auth :only (hash-password)])
        (sandbar.dev userui
                     [test :only (t)])))

(deftest test-secure-user
  (t "secure the user's password"
     (t "when the old user into in nil"
        (let [u (secure-user {:password "test"} nil)]
          (is (= (count u) 2))
          (is (not (= (:password u) "test")))
          (is (= (hash-password "test" (:salt u)) (:password u)))))
     (t "when the password has not changed"
        (let [u (secure-user {:password "jljuy"} {:password "jljuy"})]
          (is (= (:password u) "jljuy"))))
     (t "when the old user is not nil and the password has changed"
        (let [u (secure-user {:password "test"} {:password "jkluy"
                                                 :salt "jhuytrewswdq"})]
          (is (= (count u) 2))
          (is (= (:salt u) "jhuytrewswdq"))
          (is (not (= (:password u) "test")))
          (is (= (hash-password "test" "jhuytrewswdq") (:password u)))))))
