; Copyright (c) Brenton Ashworth. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file COPYING at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns sandbar.auth
  (:use (sandbar library)
        (compojure.http [routes :only (routes GET POST)]
                        [helpers :only (redirect-to)])))

(def *hash-delay* 1000)

;;
;; Helpers
;; =======
;;

(defn redirect-302 [page]
  {:status 302
   :headers {"Location" (cpath page)}
   :body ""})

(defn redirect-301 [url]
  {:status 301
   :headers {"Location" (cpath url)}})

(defn redirect-to-auth [uri-prefix]
  (redirect-302 (str uri-prefix "/login")))

(defn redirect-to-permission-denied [uri-prefix]
  (redirect-302 (str uri-prefix "/permission-denied")))

(defn params-str [request]
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
  (not (or (= x :ssl) (= x :nossl))))

(defn- role-set
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
  (let [role-part (find-matching-config (filter #(role? (last %))
                                                (partition 2 config))
                                        request)]
    (cond (keyword? role-part) (role-set role-part)
          (vector? role-part) (role-set (first role-part))
          (set? role-part) role-part)))

(defn allow-access?
  "Should the current user be allowed to access the requested resource?
   roles-fn is a function of the request which will return the roles for the
   current user."
  [config roles-fn request]
  (let [uri (:uri request)
        matching-security (required-roles config request)]
    (if matching-security
      (if (seq (clojure.set/intersection (conj (roles-fn request) :any)
                                         matching-security))
        true
        false)
      false)))

(defn auth-required?
  "Are there required roles other than :any."
  [required-roles]
  (not (and (= (count required-roles) 1)
            (= (first required-roles) :any))))


(defn filter-channel-config [config]
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
     (let [digest (java.security.MessageDigest/getInstance "SHA-1")]
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
;; Basic Authentication
;; ====================
;;

(defn create-login-from-params
  "Create a map of all login info to verify the identity of this user."
  [load-fn request]
  (let [params (:params request)
        form-data (-> (select-keys params [:username :password]))
        user (first (load-fn :app_user {:username (:username form-data)} {}))
        roles (index-by :id (load-fn :role))]
    (-> form-data
        (assoc :password-hash (:password user))
        (assoc :salt (:salt user))
        (assoc :roles (set
                       (map #(keyword (:name (roles %)))
                            (map :role_id
                                 (load-fn :user_role
                                          {:user_id (:id user)} {}))))))))

(def invalid-login?!
     (partial
      invalid?
      :login
      (fn [props form-data]
        (merge
         (required-field form-data :username
                         (str (property-lookup props :username)
                              " is required."))
         (required-field form-data :password
                         (str (property-lookup props :password)
                              " is required."))))))

(defn login-page [props request]
  (login-form
   (:uri request) "Login"
   (form-layout-grid [1 1]
                     :login
                     [(form-textfield props :username {:size 25} :required)
                      (form-password props :password {:size 25}  :required)]
                     request
                     {})))

(defn valid-password?
  ([user-data] (valid-password? user-data *hash-delay*))
  ([user-data n]
     (= (hash-password (:password user-data) (:salt user-data) n)
     (:password-hash user-data))))

(defn authenticate! [load-fn props request]
  (let [user-data (create-login-from-params load-fn request)
        success (get-from-session request :auth-redirect-uri)
        failure "login"]
    (redirect-to
     (cond (invalid-login?! props user-data request) failure
           (not (valid-password? user-data)) failure
           :else (do
                   (put-in-session! request :current-user
                                    {:name (:username user-data)
                                     :roles (:roles user-data)})
                   (remove-from-session! request :auth-redirect-uri)
                   success)))))

(defn logout! [props request]
  (let [logout-page (if-let [p (:logout-page props)]
                      (cpath p)
                      (cpath "/"))]
    (redirect-to
     (do (remove-from-session! request :current-user)
         logout-page))))

;;
;; Routes
;; ======
;;

(defn security-login-routes [path-prefix layout name-fn props data-fns]
  (routes
   (GET (str path-prefix "/login*")
        (layout (name-fn request)
                request
                (login-page props request)))
   (POST (str path-prefix "/login*")
         (authenticate! (data-fns :load) props request))
   (GET (str path-prefix "/logout*")
        (logout! props request))))


;;
;; API
;; ===
;;

(defn current-user [request]
  (get-from-session request :current-user))

(defn current-username [request]
  (:name (current-user request)))

(defn current-user-roles [request]
  (:roles (current-user request)))

(defn any-role-granted? [request & roles]
  (if (seq
       (clojure.set/intersection (current-user-roles request) (set roles)))
    true
    false))

(defn with-secure-channel
  "Middleware function to redirect to either a secure or insecure channel."
  [handler config port ssl-port]
  (fn [request]
    (let [ssl-config (filter-channel-config config)
          channel-req (find-matching-config ssl-config request)
          uri (:uri request)]
      (cond (and (= channel-req :ssl) (= (:scheme request) :http))
            {:status 301 :headers {"Location" (to-https request ssl-port)}}
            (and (= channel-req :nossl) (= (:scheme request) :https))
            {:status 301 :headers {"Location" (to-http request port)}}
            :else (handler request)))))

(defn with-security
  "Middleware function for form based authentication and authorization."
  ([handler config] (with-security handler config ""))
  ([handler config uri-prefix]
     (fn [request]
       (let [required-roles (required-roles config request)
             user (current-user request)]
         (cond (and (auth-required? required-roles)
                    (nil? user)) (do (put-in-session! request
                                                      :auth-redirect-uri
                                                      (:uri request))
                                     (redirect-to-auth uri-prefix))
                    (allow-access? config
                                   (fn [r] (:roles user))
                                   request)
                    (handler request)
                    :else (redirect-to-permission-denied uri-prefix))))))
