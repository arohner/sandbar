; Copyright (c) Brenton Ashworth. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file COPYING at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns sandbar.test_auth
  (:use [clojure.test]
        (ring.util [response :only (redirect)])
        (sandbar stateful-session library auth basic_authentication)
        [sandbar.test :only (t)]))

(deftest test-append-to-redirect-loc
  (t "append to redirect location"
     (binding [app-context (atom "")]
       (t "when append is blank"
          (is (= (append-to-redirect-loc (redirect-301 "/p") "")
                 (redirect-301 "/p"))))
       (t "when append is nil"
          (is (= (append-to-redirect-loc (redirect-301 "/p") nil)
                 (redirect-301 "/p"))))
       (t "when there is something to append"
          (is (= (append-to-redirect-loc (redirect-301 "/p") "/t")
                 (redirect-301 "/t/p"))))
       (t "does nothing when loc is a complete URL with http scheme"
          (is (= (append-to-redirect-loc (redirect-301 "http://x/p") "/t")
                 (redirect-301 "http://x/p"))))
       (t "does nothing when loc is a complete URL with https scheme"
          (is (= (append-to-redirect-loc (redirect-301 "https://x/p") "/t")
                 (redirect-301 "https://x/p")))))
     (binding [app-context (atom "/context")]
       (t "when append is blank and there is a context"
          (is (= (append-to-redirect-loc (redirect-301 "/p") "")
                 (redirect-301 "/p"))))
       (t "when there is something to append and there is a context"
          (is (= (append-to-redirect-loc (redirect-301 "/p") "/t")
                 (redirect-301 "/t/p")))))))

(def fixture-security-config
     [#"/admin.*"                 [:admin :ssl] 
      #"/idea/edit.*"             :editor
      #"/download.*"              [#{:admin :editor} :ssl]
      #"/secure.*"                :ssl
      #"/login.*"                 [:any :ssl] 
      #"/permission-denied.*"     :any
      #".*.css|.*.png"            [:any :any-channel] 
      #(.endsWith (:uri %) ".js") :any
      #".*"                       [#{:admin :user} :nossl]])

(defn create-request
  ([scheme server uri] (create-request scheme server uri ""))
  ([scheme server uri query]
     (let [port (if (= scheme :http) 8080 8443)]
       (create-request scheme server uri port query)))
  ([scheme server uri port query]
     {:uri uri
      :scheme (keyword scheme)
      :query-string query
      :server-name server
      :server-port port}))

(deftest test-with-secure-channel
  (binding [app-context (atom "")]
    (t "check channel security"
       (binding [*session* (atom {})
                 with-secure-channel (partial with-secure-channel
                                              :uri
                                              fixture-security-config
                                              8080
                                              8443)]
         (t "when scheme is http and should be https"
            (is (= ((with-secure-channel)
                    (create-request :http "host" "/admin/page"))
                   (redirect-301 "https://host:8443/admin/page"))))
         (t "when scheme is https and should be https"
            (is (= ((with-secure-channel)
                    (create-request :https "host" "/admin/page"))
                   "/admin/page")))
         (t "when scheme is http and should be http"
            (is (= ((with-secure-channel)
                    (create-request :http "host" "/standard-page"))
                   "/standard-page")))
         (t "when scheme is https and should be http"
            (is (= ((with-secure-channel)
                    (create-request :https "host" "/standard-page"))
                   (redirect-301 "http://host:8080/standard-page"))))
         (t "when ssl configured in a vector with a set"
            (is (= ((with-secure-channel)
                    (create-request :http "host" "/download/page"))
                   (redirect-301 "https://host:8443/download/page"))))
         (t "when ssl is configured by itself"
            (is (= ((with-secure-channel)
                    (create-request :http "host" "/secure/page"))
                   (redirect-301 "https://host:8443/secure/page"))))
         (t "when http -> https with request parameters"
            (is (= ((with-secure-channel)
                    (create-request :http "host" "/admin/p" "id=1&n=h"))
                   (redirect-301 "https://host:8443/admin/p?id=1&n=h"))))
         (t "when https -> http with request parameters"
            (is (= ((with-secure-channel)
                    (create-request :https "host" "/test.js" "load=x"))
                   (redirect-301 "http://host:8080/test.js?load=x"))))
         (t "when no ssl config falls through to catch all"
            (is (= ((with-secure-channel)
                    (create-request :http "host" "/test.js"))
                   "/test.js")))
         (t "when using ssl and can use any channel"
            (is (= ((with-secure-channel)
                    (create-request :https "host" "/test.css"))
                   "/test.css")))
         (t "when NOT using ssl and can use any channel"
            (is (= ((with-secure-channel)
                    (create-request :http "host" "/test.css"))
                   "/test.css")))))))
 
(deftest test-required-roles
  (t "get required roles"
     (binding [app-context (atom "")]
       (t "for an admin page with no context path"
          (is (= (required-roles fixture-security-config
                                 {:uri "/admin/page"})
                 #{:admin})))
       (t "for an editor page with no context path"
          (is (= (required-roles fixture-security-config
                                 {:uri "/idea/edit"})
                 #{:editor})))
       (t "for any page with no context path" 
          (is (= (required-roles fixture-security-config
                                 {:uri "/some/random/page"})
                 #{:admin :user})))
       (t "using a function instead of a regular expression" 
          (is (= (required-roles fixture-security-config
                                 {:uri "/script.js"})
                 #{:any})))
       (t "for a page that has an :ssl requirement" 
          (is (= (required-roles fixture-security-config
                                 {:uri "/login"})
                 #{:any})))
       (t "for a page that has ONLY an :ssl requirement but matches another re" 
          (is (= (required-roles fixture-security-config
                                 {:uri "/secure/page"})
                 #{:admin :user}))))
       (binding [app-context (atom "/test-context")]
         (t "for an admin page with context path"
           (is (= (required-roles fixture-security-config
                                  {:uri "/test-context/admin/page"})
                  #{:admin})))
         (t "for an editor page with context path"
          (is (= (required-roles fixture-security-config
                                 {:uri "/test-context/idea/edit"})
                 #{:editor})))
         (t "for any page with context path"
          (is (= (required-roles fixture-security-config
                                 {:uri "/test-context/some/random/page"})
                 #{:admin :user}))))))

(deftest test-allow-access?
  (binding [app-context (atom "")]
    (t "allow access"
      (t "when req and actual are :admin"
         (is (true? (allow-access? #{:admin} #{:admin}))))
      (t "when :admin req but user is in :user"
         (is (false? (allow-access? #{:admin} #{:user}))))
      (t "when user is black but :any is req"
         (is (true? (allow-access? #{:user :any} #{}))))
      (t "when user is black but :user is req"
         (is (false? (allow-access? #{:user} #{}))))
      (t "when user has one of two req roles"
         (is (true? (allow-access? #{:user :admin} #{:admin}))))
      (t "when user is in role but not the correct one out of two"
         (is (false? (allow-access? #{:editor :admin} #{:user})))))))

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
  (t "with security"
     (t "url config"
        (binding [app-context (atom "")
                  with-security (partial with-security
                                         :uri
                                         fixture-security-config)]
          (binding [*session* (atom {})]
            (t "redirect to login when user auth required and user is nil"
               (let [result ((with-security basic-auth)
                             {:session {:id "x"} :uri "/admin/page"})]
                 (is (= result
                        (redirect "/login")))
                 (is (= (-> @*session* :x :auth-redirect-uri)
                        "/admin/page"))))
            (t "redirect to login with a uri-prefix"
               (is (= ((with-security basic-auth "/prefix")
                       {:session {:id "x"} :uri "/admin/page"})
                      (redirect "/prefix/login"))))
            (t "allow access when auth is not required"
               (is (= ((with-security basic-auth)
                       {:uri "/test.css"})
                      "/test.css")))
            (t "and some other kind of auth, redirect to PD when not in role"
               (is (= ((with-security (fn [r] {:roles #{:user}}))
                       {:uri "/admin/page"})
                      (redirect "/permission-denied"))))
            (t "and some other kind of auth, allow access when in role"
               (is (= ((with-security (fn [r] {:roles #{:user}}))
                       {:uri "/some/page"})
                      "/some/page"))))
          (binding [*session* (atom {:x {:current-user {:name "testuser"
                                                      :roles #{:user}}}})]
            (t "redirect to permission denied when valid user without role"
               (is (= ((with-security basic-auth)
                       {:session {:id "x"} :uri "/admin/page"})
                      (redirect "/permission-denied"))))
            (t "allow access when user is in correct role"
               (is (= ((with-security basic-auth)
                       {:session {:id "x"} :uri "/some/page"})
                      "/some/page"))))))
     (t "and NO url config"
        (binding [app-context (atom "")]
          (binding [*session* (atom {})]
            (t "redirect to permission denied when access exception is thrown"
               (is (= ((with-security
                         (fn [r] (access-error "testing with-security"))
                         []
                         basic-auth)
                       {:session {:id "x"} :uri "/x"})
                      (redirect "/permission-denied"))))
            (t "redirect to login when authorization exception is thrown"
               (is (= ((with-security
                         (fn [r] (authentication-error "testing with-security"))
                         []
                         basic-auth)
                       {:session {:id "x"} :uri "/x"})
                      (redirect "/login"))))
            (binding [*session* (atom {:x {}})]
              (t "redirect to authentication error page when in auth-err loop"
                (is (= ((with-security
                          (fn [r] (if (= (:uri r) "/x")
                                    (authentication-error)
                                    "success"))
                          []
                          (fn [r] {:name "t" :roles #{:user}}))
                        {:session {:id "x"} :uri "/x"})
                       (redirect "/authentication-error")))))
            (binding [*session* (atom {:x {}})]
              (t "access page when authentication is successfull"
                (is (= ((with-security
                          (fn [r] (ensure-authenticated
                                    "success"))
                          []
                          (fn [r] {:name "t" :roles #{:user}}))
                        {:session {:id "x"} :uri "/x"})
                       "success"))))
            (binding [*session* (atom {:x {}})]
              (t "permission denied using ensure-any-role"
                 (is (= ((with-security
                           (fn [r] (ensure-any-role [:admin]
                                                    "success"))
                           []
                           (fn [r] {:name "t" :roles #{:user}}))
                         {:session {:id "x"} :uri "/x"})
                        (redirect "/permission-denied")))))
            (binding [*session* (atom {:x {}})]
              (t "access allowed using ensure-any-role"
                 (is (= ((with-security
                           (fn [r] (ensure-any-role [:user]
                                                    "success"))
                           []
                           (fn [r] {:name "t" :roles #{:user}}))
                         {:session {:id "x"} :uri "/x"})
                        "success"))))
            (binding [*session* (atom {:x {}})]
              (t "access-allowed using ensure-any-role when multiple roles"
                 (is (= ((with-security
                           (fn [r] (ensure-any-role [:admin :user]
                                                    "success"))
                           []
                           (fn [r] {:name "t" :roles #{:user}}))
                         {:session {:id "x"} :uri "/x"})
                        "success")))))))))

(deftest test-any-role-granted?
  (t "are any of these roles granted"
     (let [req {:session {:id "x"}}]
       (binding [*session* (atom {:x {:current-user {:name "testuser"
                                                   :roles #{:user}}}})]
         (t "passing the request and role is missing"
            (is (false? (any-role-granted? req :admin))))
         (t "passing the request and one matching role"
            (is (true? (any-role-granted? req :user)))))
       (binding [*session* (atom {:x {:current-user {:name "testuser"
                                                   :roles #{:user :admin}}}})]
         (t "passing the request and one of two roles match"
            (is (true? (any-role-granted? req :admin))))
         (t "passing the request and another of the two roles match"
            (is (true? (any-role-granted? req :user)))))
       (binding [*session* (atom {})
                 *current-user* {:name "testuser"
                                 :roles #{:admin}}]
         (t "using *current-user* binding with a matching role"
            (is (true? (any-role-granted? :admin))))
         (t "using *current-user* binding and no matching role"
            (is (false? (any-role-granted? :user))))))))
