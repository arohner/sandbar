;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.dev.test-basic-authentication
  (:use [clojure.test]
        (ring.util [response :only (redirect)])
        (sandbar core
                 stateful-session
                 auth
                 test-fixtures)
        (sandbar.dev basic-authentication
                     [test :only (t)])))

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
                                         {"username" "u"
                                          "password" "test"})
               {:username "u" :password "test" :salt "cfjhuy"
                :password-hash (hash-password "test" "cfjhuy")
                :roles #{:admin}})))))

(deftest test-authenticate!
  (t "authenticate!"
     (binding [*sandbar-session* (atom {})]
       (t "with missing username"
          (is (= (authenticate! test-login-load-fn
                                {}
                                {"password" "x"})
                 (redirect "login"))))
       (t "with missing password"
        (is (= (authenticate! test-login-load-fn
                              {}
                              {"username" "u"})
               (redirect "login")))))
     (t "with correct password"
        (binding [*sandbar-session* (atom {:auth-redirect-uri "/test"})]
          (let [result (authenticate! test-login-load-fn
                                      {}
                                      {"username" "u" "password" "test"})]
            (is (= result
                   (redirect "/test")))
            (is (= @*sandbar-session*
                   {:current-user {:name "u"
                                   :roles #{:admin}}})))))
     (t "with incorrect password"
        (binding [*sandbar-session* (atom {:auth-redirect-uri "/test"})]
          (let [result (authenticate! test-login-load-fn
                                      {}
                                      {"username" "u" "password" "wrong"})]
            (is (= result
                   (redirect "login")))
            (is (= (:auth-redirect-uri @*sandbar-session*)
                   "/test")))))))

(deftest test-with-security-with-basic-auth
  (binding [*sandbar-session* (atom {})]
    (t "with security using basic auth"
       (t "url config"
         (binding [app-context (atom "")
                   with-security (partial with-security
                                          :uri
                                          fixture-security-config)]
           (t "redirect to login when user auth required and user is nil"
              (let [result ((with-security basic-auth)
                            {:uri "/admin/page"})]
                (is (= result
                       (redirect "/login")))
                (is (= (:auth-redirect-uri @*sandbar-session*)
                       "/admin/page"))))
           (t "redirect to login with a uri-prefix"
              (is (= ((with-security basic-auth "/prefix")
                      {:uri "/admin/page"})
                     (redirect "/prefix/login"))))
           (t "allow access when auth is not required"
              (is (= ((with-security basic-auth)
                      {:uri "/test.css"})
                     "/test.css")))
           (binding [*sandbar-session* (atom {:current-user {:name "testuser"
                                                             :roles #{:user}}})]
             (t "redirect to permission denied when valid user without role"
              (is (= ((with-security basic-auth)
                      {:uri "/admin/page"})
                     (redirect "/permission-denied"))))
             (t "allow access when user is in correct role"
              (is (= ((with-security basic-auth)
                      {:uri "/some/page"})
                     "/some/page"))))))
      (t "and NO url config"
         (binding [app-context (atom "")]
           (t "redirect to permission denied when access exception is thrown"
              (is (= ((with-security
                        (fn [r] (access-error "testing with-security"))
                        []
                        basic-auth)
                      {:uri "/x"})
                     (redirect "/permission-denied"))))
           (t "redirect to login when authorization exception is thrown"
              (is (= ((with-security
                        (fn [r] (authentication-error "testing with-security"))
                        []
                        basic-auth)
                      {:uri "/x"})
                     (redirect "/login")))))))))
