; Copyright (c) Brenton Ashworth. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file COPYING at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns sandbar.test_auth
  (:use [clojure.test]
        (sandbar library auth basic_authentication)
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
       (binding [session (atom {})
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
    (t "access to a uri"
      (t "IS allowed when role is found for uri"
         (is (= (allow-access? fixture-security-config
                               (fn [r] #{:admin})
                               {:uri "/admin/page"})
                true)))
      (t "is NOT allowed when no app context is set"
         (is (= (allow-access? fixture-security-config
                               (fn [r] #{:editor})
                               {:uri "/test-context/idea/edit"})
                false)))
      (t "IS allowed with the app-context is set"
         (binding [app-context (atom "/test-context")]
           (is (= (allow-access? fixture-security-config
                                 (fn [r] #{:editor})
                                 {:uri "/test-context/idea/edit"})
                  true))))
      (t "is NOT allowed when user is not in correct role"
         (is (= (allow-access? fixture-security-config
                               (fn [r] #{:user :editor})
                               {:uri "/admin/page"})
                false)))
      (t "IS allowed when user is in one of the correct roles"
         (is (= (allow-access? fixture-security-config
                               (fn [r] #{:user :editor})
                               {:uri "/index"})
                true)))
      (t "IS allowed when user is in one of the two allowed roles"
         (is (= (allow-access? fixture-security-config
                               (fn [r] #{:editor})
                               {:uri "/download"})
                true)))
      (t "is NOT allowed when user is not in the correct role"
         (is (= (allow-access? fixture-security-config
                               (fn [r] #{:user})
                               {:uri "/download"})
                false)))
      (t "is Not allowed when user is not in the correct role"
         (is (= (allow-access? fixture-security-config
                               (fn [r] #{:user})
                               {:uri "/admin/page"})
                false)))
      (t "is Not allowed when no matching entry is found"
         (is (= (allow-access? (take 1 fixture-security-config)
                               (fn [r] #{:user})
                               {:uri "/index"})
                false)))
      (t "IS allowed when the role is set to :any"
         (is (= (allow-access? fixture-security-config
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
  (binding [app-context (atom "")
            with-security (partial with-security :uri fixture-security-config)]
    (t "with security"
       (binding [session (atom {})]
         (t "redirect to login when user auth required and user is nil"
            (let [result ((with-security basic-auth)
                           {:session {:id "x"} :uri "/admin/page"})]
               (is (= result
                      (redirect-302 "/login")))
               (is (= (-> @session :x :auth-redirect-uri)
                      "/admin/page"))))
         (t "redirect to login with a uri-prefix"
            (is (= ((with-security basic-auth "/prefix")
                    {:session {:id "x"} :uri "/admin/page"})
                   (redirect-302 "/prefix/login"))))
          (t "allow access when auth is not required"
             (is (= ((with-security basic-auth)
                     {:uri "/test.css"})
                    "/test.css")))
          (t "and some other kind of auth, redirect to PD when not in role"
             (is (= ((with-security (fn [r] {:roles #{:user}}))
                     {:uri "/admin/page"})
                    (redirect-302 "/permission-denied"))))
          (t "and some other kind of auth, allow access when in role"
             (is (= ((with-security (fn [r] {:roles #{:user}}))
                     {:uri "/some/page"})
                    "/some/page"))))
        (binding [session (atom {:x {:current-user {:name "testuser"
                                                    :roles #{:user}}}})]
          (t "redirect to permission denied when valid user without role"
             (is (= ((with-security basic-auth)
                     {:session {:id "x"} :uri "/admin/page"})
                    (redirect-302 "/permission-denied"))))
          (t "allow access when user is in correct role"
             (is (= ((with-security basic-auth)
                     {:session {:id "x"} :uri "/some/page"})
                    "/some/page")))))))

