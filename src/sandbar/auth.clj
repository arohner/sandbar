; Copyright (c) Brenton Ashworth. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file COPYING at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns sandbar.auth
  (:use (sandbar [library :only (cpath
                                 remove-cpath
                                 get-from-session)])))

(def *hash-delay* 1000)

;;
;; Helpers - Pure Functions
;; ========================
;;

(defn redirect-302 [page]
  {:status 302
   :headers {"Location" (cpath page)}
   :body ""})

(defn redirect-301 [url]
  {:status 301
   :headers {"Location" (cpath url)}})

(defn redirect? [m]
  (or (= (:status m) 302)
      (= (:status m) 301)))

(defn append-to-redirect-loc
  "Append the uri-prefix to the value of Location in the headers of the
   redirect map."
  [m uri-prefix]
  (if (or (nil? uri-prefix) (empty? uri-prefix))
    m
    (let [loc (remove-cpath ((:headers m) "Location"))]
      (if (re-matches #".*://.*" loc)
        m
        (merge m {:headers {"Location" (cpath (str uri-prefix loc))}})))))

(defn- redirect-to-permission-denied [uri-prefix]
  (redirect-302 (str uri-prefix "/permission-denied")))

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
  "Middleware function for authentication and authorization."
  ([handler config auth-fn] (with-security handler config auth-fn ""))
  ([handler config auth-fn uri-prefix]
     (fn [request]
       (let [required-roles (required-roles config request)
             user (current-user request)
             user-status (if (and (auth-required? required-roles)
                                  (nil? user))
                           (auth-fn request)
                           user)]
         (cond (redirect? user-status)
               (append-to-redirect-loc user-status uri-prefix)
               (allow-access? config
                              (fn [r] (:roles user-status))
                              request)
               (handler request)
               :else (redirect-to-permission-denied uri-prefix))))))
             
