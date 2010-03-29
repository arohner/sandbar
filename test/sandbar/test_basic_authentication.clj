; Copyright (c) Brenton Ashworth. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file COPYING at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns sandbar.test_basic_authentication
  (:use [clojure.test]
        (ring.util [response :only (redirect)])
        (sandbar library auth basic_authentication)
        [sandbar.test :only (t)]))

(defn test-login-load-fn
  ([k] (test-login-load-fn k {} {}))
  ([k m1 m2]
     (cond (= k :app_user) [{:id 1 :username "u"
                             :password (hash-password "test" "cfjhuy")
                             :salt "cfjhuy"}] 
           (= k :role) [{:id 1 :name "admin"} {:id 2 :name "user"}]
           (= k :user_role) [{:user_id 1 :role_id 1}]
           :else {})))

(deftest test-create-login-from-params
  (t "create login from params"
     (t "with valid username"
        (is (= (create-login-from-params test-login-load-fn
                                         {:params {:username "u"
                                                   :password "test"}})
               {:username "u" :password "test" :salt "cfjhuy"
                :password-hash (hash-password "test" "cfjhuy")
                :roles #{:admin}})))))

(deftest test-authenticate!
  (t "authenticate!"
     (t "with missing username"
        (is (= (authenticate! test-login-load-fn
                              {}
                              {:params {:password "x"}})
               (redirect "login"))))
     (t "with missing password"
        (is (= (authenticate! test-login-load-fn
                              {}
                              {:params {:username "u"}})
               (redirect "login"))))
     (t "with correct password"
        (binding [session (atom {:x {:auth-redirect-uri "/test"}})]
          (let [result (authenticate! test-login-load-fn
                                      {}
                                      {:session {:id "x"}
                                       :params
                                       {:username "u" :password "test"}})]
            (is (= result
                   (redirect "/test")))
            (is (= (-> @session
                       :x
                       (dissoc :last-access))
                   {:current-user {:name "u"
                                   :roles #{:admin}}})))))
     (t "with incorrect password"
        (binding [session (atom {:x {:auth-redirect-uri "/test"}})]
          (let [result (authenticate! test-login-load-fn
                                      {}
                                      {:session {:id "x"}
                                       :params
                                       {:username "u" :password "wrong"}})]
            (is (= result
                   (redirect "login")))
            (is (= (-> @session
                       :x
                       (dissoc :last-access))
                   {:auth-redirect-uri "/test"})))))))
