; Copyright (c) Brenton Ashworth. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns sandbar.test_security
  (:use [clojure.test]
        (sandbar library security)
        [sandbar.test :only (t)]))

;; you have not tested all of these setting since changeing this.
(def test-security-config
     [#"/admin.*"                 [:admin :ssl] 
      #"/idea/edit.*"             :editor
      #"/download.*"              [#{:admin :editor} :ssl]
      #"/secure.*"                :ssl
      #"/login.*"                 [:any :ssl] 
      #"/permission-denied.*"     :any
      #".*.css|.*.png"            :any
      #(.endsWith (:uri %) ".js") :any
      #".*"                       [#{:admin :user} :nossl]])

(deftest test-with-secure-channel
  (binding [app-context (atom "")]
    (t "check channel security"
       (binding [session (atom {})]
         (t "when scheme is http and should be https"
            (is (= ((with-secure-channel :uri test-security-config 8080 8443)
                    {:uri "/admin/page" :scheme :http
                     :headers {"host" "host:8080"} })
                   {:status 301
                    :headers {"Location" "https://host:8443/admin/page"}})))
         (t "when scheme is https and should be https"
            (is (= ((with-secure-channel :uri test-security-config 8080 8443)
                    {:uri "/admin/page" :scheme :https
                     :headers {"host" "host:8443"} })
                   "/admin/page")))
         (t "when scheme is http and should be http"
            (is (= ((with-secure-channel :uri test-security-config 8080 8443)
                    {:uri "/standard-page" :scheme :http
                     :headers {"host" "host:8080"} })
                   "/standard-page")))
         (t "when scheme is https and should be http"
            (is (= ((with-secure-channel :uri test-security-config 8080 8443)
                    {:uri "/standard-page" :scheme :https
                     :headers {"host" "host:8443"} })
                   {:status 301
                    :headers {"Location" "http://host:8080/standard-page"}})))
         (t "when ssl configured in a vector with a set"
            (is (= ((with-secure-channel :uri test-security-config 8080 8443)
                    {:uri "/download/page" :scheme :http
                     :headers {"host" "host:8080"} })
                   {:status 301
                    :headers {"Location" "https://host:8443/download/page"}})))
         (t "when ssl configured by itself"
            (is (= ((with-secure-channel :uri test-security-config 8080 8443)
                    {:uri "/secure/page" :scheme :http
                     :headers {"host" "host:8080"} })
                   {:status 301
                    :headers {"Location" "https://host:8443/secure/page"}})))
         (t "when no ssl config falls through to catch all"
            (is (= ((with-secure-channel :uri test-security-config 8080 8443)
                    {:uri "/test.js" :scheme :http
                     :headers {"host" "host:8080"}})
                   "/test.js")))))))
 
(deftest test-required-roles
  (t "get required roles"
     (binding [app-context (atom "")]
       (t "for an admin page with no context path"
          (is (= (required-roles test-security-config
                                 {:uri "/admin/page"})
                 #{:admin})))
       (t "for an editor page with no context path"
          (is (= (required-roles test-security-config
                                 {:uri "/idea/edit"})
                 #{:editor})))
       (t "for any page with no context path" 
          (is (= (required-roles test-security-config
                                 {:uri "/some/random/page"})
                 #{:admin :user})))
       (t "using a function instead of a regular expression" 
          (is (= (required-roles test-security-config
                                 {:uri "/script.js"})
                 #{:any})))
       (t "for a page that has an :ssl requirement" 
          (is (= (required-roles test-security-config
                                 {:uri "/login"})
                 #{:any})))
       (t "for a page that has ONLY an :ssl requirement but matches another re" 
          (is (= (required-roles test-security-config
                                 {:uri "/secure/page"})
                 #{:admin :user}))))
       (binding [app-context (atom "/test-context")]
         (t "for an admin page with context path"
           (is (= (required-roles test-security-config
                                  {:uri "/test-context/admin/page"})
                  #{:admin})))
         (t "for an editor page with context path"
          (is (= (required-roles test-security-config
                                 {:uri "/test-context/idea/edit"})
                 #{:editor})))
         (t "for any page with context path"
          (is (= (required-roles test-security-config
                                 {:uri "/test-context/some/random/page"})
                 #{:admin :user}))))))

(deftest test-allow-access?
  (binding [app-context (atom "")]
    (t "access to a uri"
      (t "IS allowed when role is found for uri"
         (is (= (allow-access? test-security-config
                               (fn [r] #{:admin})
                               {:uri "/admin/page"})
                true)))
      (t "is NOT allowed when no app context is set"
         (is (= (allow-access? test-security-config
                               (fn [r] #{:editor})
                               {:uri "/test-context/idea/edit"})
                false)))
      (t "IS allowed with the app-context is set"
         (binding [app-context (atom "/test-context")]
           (is (= (allow-access? test-security-config
                                 (fn [r] #{:editor})
                                 {:uri "/test-context/idea/edit"})
                  true))))
      (t "is NOT allowed when user is not in correct role"
         (is (= (allow-access? test-security-config
                               (fn [r] #{:user :editor})
                               {:uri "/admin/page"})
                false)))
      (t "IS allowed when user is in one of the correct roles"
         (is (= (allow-access? test-security-config
                               (fn [r] #{:user :editor})
                               {:uri "/index"})
                true)))
      (t "IS allowed when user is in one of the two allowed roles"
         (is (= (allow-access? test-security-config
                               (fn [r] #{:editor})
                               {:uri "/download"})
                true)))
      (t "is NOT allowed when user is not in the correct role"
         (is (= (allow-access? test-security-config
                               (fn [r] #{:user})
                               {:uri "/download"})
                false)))
      (t "is Not allowed when user is not in the correct role"
         (is (= (allow-access? test-security-config
                               (fn [r] #{:user})
                               {:uri "/admin/page"})
                false)))
      (t "is Not allowed when no matching entry is found"
         (is (= (allow-access? (take 1 test-security-config)
                               (fn [r] #{:user})
                               {:uri "/index"})
                false)))
      (t "IS allowed when the role is set to :any"
         (is (= (allow-access? test-security-config
                               (fn [r] #{:none})
                               {:uri "/permission-denied"})
                true))))))

(deftest test-auth-required?
  (t "auth required"
     (t "when role is :admin"
        (is (= (auth-required? #{:admin}))
            true))
     (t "when role is :user"
        (is (= (auth-required? #{:user}))
            true))
     (t "user there is more than one role"
        (is (= (auth-required? #{:admin :user}))
            true))
     (t "when role is any"
        (is (= (auth-required? #{:any}))
            false))
     (t "when role set is empty"
        (is (= (auth-required? #{}))
            true))
     (t "when role set is nil"
        (is (= (auth-required? nil))
            true))))

(deftest test-with-security
  (binding [app-context (atom "")]
    (t "with security"
       (binding [session (atom {})]
         (t "redirect to login when user auth required and user is nil"
            (let [result ((with-security :uri test-security-config)
                          {:session {:id "x"} :uri "/admin/page"})]
              (is (= result
                     (redirect-template "/login")))
              (is (= (-> @session :x :auth-redirect-uri)
                     "/admin/page"))))
         (t "allow access when auth is not required"
            (is (= ((with-security :uri test-security-config)
                    {:uri "/test.css"})
                   "/test.css"))))
       (binding [session (atom {:x {:current-user {:name "testuser"
                                                   :roles #{:user}}}})]
         (t "redirect to permission denied when valid user without role"
            (is (= ((with-security :uri test-security-config)
                    {:session {:id "x"} :uri "/admin/page"})
                   (redirect-template "/permission-denied"))))
         (t "allow access when user is in correct role"
            (is (= ((with-security :uri test-security-config)
                    {:session {:id "x"} :uri "/some/page"})
                   "/some/page")))))))

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
               [302 {:headers {"Location" "login"}}])))
     (t "with missing password"
        (is (= (authenticate! test-login-load-fn
                              {}
                              {:params {:username "u"}})
               [302 {:headers {"Location" "login"}}])))
     (t "with correct password"
        (binding [session (atom {:x {:auth-redirect-uri "/test"}})]
          (let [result (authenticate! test-login-load-fn
                                      {}
                                      {:session {:id "x"}
                                       :params
                                       {:username "u" :password "test"}})]
            (is (= result
                   [302 {:headers {"Location" "/test"}}]))
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
                   [302 {:headers {"Location" "login"}}]))
            (is (= (-> @session
                       :x
                       (dissoc :last-access))
                   {:auth-redirect-uri "/test"})))))))

