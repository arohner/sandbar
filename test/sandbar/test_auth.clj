;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.test_auth
  (:use [clojure.test]
        (clojure.contrib [error-kit :as kit])
        (ring.util [response :only (redirect)])
        (sandbar library auth stateful-session test-fixtures)
        [sandbar.test :only (t)]))

;;
;; Helpers
;; =======
;;

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

;;
;; Tests
;; =====
;;

(deftest test-role-set
  (is (= (role-set #{:user})
         #{:user}))
  (is (= (role-set :user)
         #{:user}))
  (is (nil? (role-set :ssl)))
  (is (nil? (role-set :nossl)))
  (is (nil? (role-set :any-channel))))

(deftest test-find-matching-config
  (t "test not implemented"
     (is (true? false))))

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

(deftest test-filter-channel-config
  (t "test not implemented"
     (is (true? false))))

(deftest test-hash-password
  (is "test not implemented"
      (true? false)))

(deftest test-any-role-granted?
  (t "are any of these roles granted"
     (binding [*sandbar-current-user* {:name "testuser" :roles #{:user}}]
       (t "passing the request and role is missing"
          (is (false? (any-role-granted? :admin))))
       (t "passing the request and one matching role"
          (is (true? (any-role-granted? :user)))))
     (binding [*sandbar-current-user* {:name "testuser"
                                       :roles #{:user :admin}}]
       (t "passing the request and one of two roles match"
          (is (true? (any-role-granted? :admin))))
       (t "passing the request and another of the two roles match"
          (is (true? (any-role-granted? :user)))))
     (binding [*sandbar-current-user* {:name "testuser" :roles #{:admin}}]
       (t "using *sandbar-current-user* binding with a matching role"
          (is (true? (any-role-granted? :admin))))
       (t "using *sandbar-current-user* binding and no matching role"
          (is (false? (any-role-granted? :user)))))))

(defmacro auth-error->false [& body]
  `(kit/with-handler
     (do ~@body)
     (kit/handle *authentication-error* [n#] false)))

(defmacro access-error->false [& body]
  `(kit/with-handler
     (do ~@body)
     (kit/handle *access-error* [n#] false)))

(defn test-handler-fn
  ([x] (test-handler-fn x "success"))
  ([x result]
     (swap! x (fn [a b] b) result)
     @x))

(defn auth-failure [test-fn]
  (let [a (atom nil)
        result (auth-error->false
                (test-fn (partial test-handler-fn a)))]
    (is (false? result))
    (is (nil? @a))))

(defn access-failure [test-fn]
  (let [a (atom nil)
        result (access-error->false
                (test-fn (partial test-handler-fn a)))]
    (is (false? result))
    (is (nil? @a))))

(defn auth-success [test-fn]
  (let [a (atom nil)]
    (auth-error->false
     (is (= (test-fn (partial test-handler-fn a))
            "success")))))

(defn access-success [test-fn]
  (let [a (atom nil)]
    (access-error->false
     (is (= (test-fn (partial test-handler-fn a))
            "success")))))

(deftest test-ensure-authenticated
  (binding [*sandbar-current-user* nil]
    (is (auth-failure #(ensure-authenticated (%)))))
  (binding [*sandbar-current-user* {:roles #{:user}}]
    (is (auth-success #(ensure-authenticated (%))))))

(deftest test-ensure-any-role
  (binding [*sandbar-current-user* {:roles #{:user}}]
    (is (access-failure #(ensure-any-role #{:admin} (%))))
    (is (access-success #(ensure-any-role #{:admin :user} (%))))
    (is (access-success #(ensure-any-role #{:user} (%)))))
  (binding [*sandbar-current-user* nil]
    (is (auth-failure #(ensure-any-role #{:admin} (%))))))

(deftest test-ensure-any-role-if
  (let [x :a]
    (t "ensure any role if"
       (binding [*sandbar-current-user* {:roles #{:user}}]
         (t "when user is not in role"
            (is (access-failure #(ensure-any-role-if (= x :a) #{:admin} (%)))))
         (t "when user is in role"
            (is (access-success #(ensure-any-role-if (= x :a) #{:user} (%)))))
         (t "when pred is false"
            (is (access-success #(ensure-any-role-if (= x :b) #{:admin} (%)))))
         (t "with multiple preds"
            (t "and user is in roles for second pred"
               (is (access-success #(ensure-any-role-if (= x :b) #{:admin}
                                                        (= x :a) #{:user}
                                                        (%)))))
            (t "and no true preds"
               (is (access-failure #(ensure-any-role-if (= x :a) #{:admin}
                                                        (= x :b) #{:user}
                                                        (%)))))
            (t "and user is in roles for the first pred"
               (is (access-success #(ensure-any-role-if (= x :a) #{:user}
                                                        (= x :c) #{:admin}
                                                        (%)))))))
       (t "user is nil"
          (binding [*sandbar-current-user* nil]
            (t "and authorization passes because of no matching pred"
               (is (auth-success #(ensure-any-role-if (= x :b) #{:admin}
                                                      (%)))))
            (t "and auth fails"
               (is (auth-failure #(ensure-any-role-if (= x :a) #{:admin}
                                                      (%)))))
            (t "with multiple preds"
               (t "where the second pred is true and auth fails"
                  (is (auth-failure #(ensure-any-role-if (= x :b) #{:admin}
                                                         (= x :a) #{:user}
                                                         (%)))))
               (t "where auth is successful because no preds match"
                  (is (auth-success #(ensure-any-role-if (= x :b) #{:admin}
                                                         (= x :c) #{:user}
                                                         (%)))))))))))

(deftest test-with-secure-channel
  (binding [app-context (atom "")]
    (t "check channel security"
       (binding [with-secure-channel (partial with-secure-channel
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

(deftest test-with-security
  (t "with security"
     (t "url config"
        (binding [*sandbar-session* (atom {})
                  app-context (atom "")
                  with-security (partial with-security
                                         :uri
                                         fixture-security-config)]
          (t "and some other kind of auth, redirect to PD when not in role"
             (is (= ((with-security (fn [r] {:roles #{:user}}))
                     {:uri "/admin/page"})
                    (redirect "/permission-denied"))))
          (t "and some other kind of auth, allow access when in role"
             (is (= ((with-security (fn [r] {:roles #{:user}}))
                     {:uri "/some/page"})
                    "/some/page")))))
     (t "and NO url config"
        (binding [*sandbar-session* (atom {})
                  app-context (atom "")]
          (t "redirect to authentication error page when in auth-err loop"
             (is (= ((with-security
                       (fn [r] (if (= (:uri r) "/x")
                                 (authentication-error)
                                 "success"))
                       []
                       (fn [r] {:name "t" :roles #{:user}}))
                     {:uri "/x"})
                    (redirect "/authentication-error"))))
          (t "access page when authentication is successfull"
             (is (= ((with-security
                       (fn [r] (ensure-authenticated
                                "success"))
                       []
                       (fn [r] {:name "t" :roles #{:user}}))
                     {:uri "/x"})
                    "success")))
          (t "permission denied using ensure-any-role"
             (is (= ((with-security
                       (fn [r] (ensure-any-role [:admin]
                                                "success"))
                       []
                       (fn [r] {:name "t" :roles #{:user}}))
                     {:uri "/x"})
                    (redirect "/permission-denied"))))
          (t "access allowed using ensure-any-role"
             (is (= ((with-security
                       (fn [r] (ensure-any-role [:user]
                                                "success"))
                       []
                       (fn [r] {:name "t" :roles #{:user}}))
                     {:uri "/x"})
                    "success")))
          (t "access-allowed using ensure-any-role when multiple roles"
             (is (= ((with-security
                       (fn [r] (ensure-any-role [:admin :user]
                                                "success"))
                       []
                       (fn [r] {:name "t" :roles #{:user}}))
                     {:uri "/x"})
                    "success")))))))


