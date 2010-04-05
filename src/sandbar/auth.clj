;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.auth
  (:use (ring.util [response :only (redirect)])
        (clojure [set :only (intersection)])
        (clojure.contrib [error-kit :as kit])
        (sandbar [core :only (remove-cpath
                              redirect?
                              redirect-301
                              append-to-redirect-loc)]
                 stateful-session)))

(def *hash-delay* 1000)

(def *sandbar-current-user* nil)

(kit/deferror *access-error* [] [n]
  {:msg (str "Access error: " n)
   :unhandled (kit/throw-msg Exception)})

(kit/deferror *authentication-error* [] [n]
  {:msg (str "Authentication error: " n)
   :unhandled (kit/throw-msg Exception)})

(defn- redirect-to-permission-denied [uri-prefix]
  (redirect (str uri-prefix "/permission-denied")))

(defn- redirect-to-authentication-error [uri-prefix]
  (redirect (str uri-prefix "/authentication-error")))

(defn- params-str [request]
  (let [p (:query-string request)]
    (if (not (empty? p)) (str "?" p) "")))

(defn to-https [request ssl-port]
  (let [host (:server-name request)]
    (str "https://" host ":" ssl-port (:uri request)
         (params-str request))))

(defn to-http [request port]
  (let [host (:server-name request)
        port (if (= port 80) "" (str ":" port))]
    (str "http://" host port (:uri request)
         (params-str request))))

(defn- role? [x]
  (not (or (= x :ssl) (= x :nossl) (= x :any-channel))))

(defn role-set
  "Return a set of roles or nil. The input could be a single role, a set of
   roles, :ssl or :nossl. The last two are not roles."
  [role]
  (cond (not (role? role)) nil
        (keyword? role) #{role}
        (set? role) role
        :else nil))

(defn find-matching-config
  "Find the configuration that matches the current uri."
  [coll request]
  (let [uri (:uri request)]
    (last
     (first
      (filter #(let [pred (first %)]
                 (if (fn? pred)
                   (pred request)
                   (re-matches pred (remove-cpath uri))))
              coll)))))

(defn required-roles
  "Get the set of roles that a user is required to have for the requested
   resource."
  [config request]
  (if (or (nil? config) (empty? config))
    #{:any}
    (let [role-part (find-matching-config (filter #(role? (last %))
                                                  (partition 2 config))
                                          request)]
      (cond (keyword? role-part) (role-set role-part)
            (vector? role-part) (role-set (first role-part))
            (set? role-part) role-part))))

(defn intersect-exists? [s1 s2]
  (not (empty? (intersection s1 s2))))

(defn allow-access?
  "Does user-roles plus :any contain any of the roles in required-roles?"
  [required-roles user-roles]
  (if required-roles
    (intersect-exists? (set (conj user-roles :any)) required-roles)
    false))

(defn auth-required?
  "Are there required roles other than :any."
  [required-roles]
  (not (and (= (count required-roles) 1)
            (= (first required-roles) :any))))

(defn filter-channel-config
  "Extract the channel configuration from the security configuration."
  [config]
  (map #(vector (first %) (if (vector? (last %))
                            (last (last %))
                            (last %)))
       (filter #(cond (and (keyword? (last %))
                           (not (role? (last %))))
                      true
                      (and (vector? (last %))
                           (not (role? (last (last %)))))
                      true
                      :else false)
               (partition 2 config))))

(defn hash-password
  "Generate a hash from the password and salt. Default value of n is
   *hash-delay*."
  ([password salt] (hash-password password salt *hash-delay*))
  ([password salt n]
     (let [digest (java.security.MessageDigest/getInstance "SHA-256")]
       (do (.reset digest)
           (.update digest (.getBytes salt "UTF-8")))
       (loop [input (.digest digest (.getBytes password "UTF-8"))
              count n]
         (if (= count 0)
           (apply str input)
           (recur (do (.reset digest)
                      (.digest digest input))
                  (dec count)))))))

;;
;; API
;; ===
;;

(defn current-user []
  (or *sandbar-current-user* (session-get :current-user)))

(defn current-username []
  (:name (current-user)))

(defn current-user-roles []
  (:roles (current-user)))

(defn any-role-granted?
  "Determine if any of the passed roles are granted. The first argument must
   be the request unless we are running in a context in which
   *sandbar-current-user* is defined."
  [& roles]
  (let [user-roles (current-user-roles)]
    (intersect-exists? user-roles (set roles))))

(defn access-error
  ([] (access-error "Access Denied!"))
  ([n] (kit/raise *access-error* n)))

(defn authentication-error
  ([] (authentication-error "No Authenticated User!"))
  ([n] (kit/raise *authentication-error* n)))

(defmacro ensure-authenticated [& body]
  `(if *sandbar-current-user*
     (do ~@body)
     (authentication-error)))

(defmacro ensure-any-role [roles & body]
  `(ensure-authenticated
    (if (apply any-role-granted? ~roles)
      (do ~@body)
      (access-error (str "The user "
                         (current-username)
                         " is not in one of "
                         ~roles)))))

(defmacro ensure-any-role-if [& clauses]
  (if (odd? (count clauses))
    (if (= 1 (count clauses))
      (first clauses)
      (list 'if (first clauses)
            (list 'sandbar.auth/ensure-any-role (second clauses) (last clauses))
            (cons 'sandbar.auth/ensure-any-role-if (drop 2 clauses))))
    (throw (IllegalArgumentException.
            "ensure-any-role-if must have an odd number of forms"))))

(defn with-secure-channel
  "Middleware function to redirect to either a secure or insecure channel."
  [handler config port ssl-port]
  (fn [request]
    (let [ssl-config (filter-channel-config config)
          channel-req (find-matching-config ssl-config request)
          uri (:uri request)]
      (cond (and (= channel-req :ssl) (= (:scheme request) :http))
            (redirect-301 (to-https request ssl-port))
            (and (= channel-req :nossl) (= (:scheme request) :https))
            (redirect-301 (to-http request port))
            :else (handler request)))))

(defn with-security
  "Middleware function for authentication and authorization."
  ([handler config auth-fn] (with-security handler config auth-fn ""))
  ([handler config auth-fn uri-prefix]
     (fn [request]
       (let [required-roles (required-roles config request)
             user (current-user)
             user-status (if (and (auth-required? required-roles)
                                  (nil? user))
                           (auth-fn request)
                           user)]
         (cond (redirect? user-status)
               (append-to-redirect-loc user-status uri-prefix)
               (allow-access? required-roles (:roles user-status))
               (binding [*sandbar-current-user* user-status]
                 (kit/with-handler
                   (handler request)
                   (kit/handle *access-error* [n]
                               (redirect-to-permission-denied uri-prefix))
                   (kit/handle *authentication-error* [n]
                               (if *sandbar-current-user*
                                 (redirect-to-authentication-error uri-prefix)
                                 (let [user-status (auth-fn request)]
                                   (if (redirect? user-status)
                                     (append-to-redirect-loc user-status
                                                             uri-prefix)
                                     (do (session-put! :current-user
                                                       user-status)
                                         (set! *sandbar-current-user*
                                               user-status)
                                         (handler request))))))))
               :else (redirect-to-permission-denied uri-prefix))))))
