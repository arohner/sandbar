; Copyright (c) Brenton Ashworth. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns sandbar.security
  (:use [compojure.html]
        [compojure.http.helpers :only (redirect-to)]
        [compojure.http [routes :only (routes GET POST)]
                        [helpers :only (redirect-to)]
                        [session :only (flash-assoc)]]
        [clojure.contrib.str-utils :only (re-split re-partition)]
        (sandbar library)
        [sandbar.database :as database]))

(def *hash-delay* 1000)

(defn- role? [x]
  (not (or (= x :ssl) (= x :nossl))))

(defn- role-set
  "Return a set of roles or nil. The input could be a single role, a set of
   roles or :ssl which is not a role."
  [role]
  (cond (not (role? role)) nil
        (keyword? role) #{role}
        (set? role) role
        :else nil))

(defn find-matching-config [coll request]
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
  "Should the current user be allowed to access the requested resource. roles-fn
   is a function of the request which will return the roles for the current
   user."
  [config roles-fn request]
  (let [uri (:uri request)
        matching-security (required-roles config request)]
    (if matching-security
      (if (seq (clojure.set/intersection (conj (roles-fn request) :any)
                                         matching-security))
        true
        false)
      false)))

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

(defn auth-required?
  "Are there required roles other than :any."
  [required-roles]
  (not (and (= (count required-roles) 1)
            (= (first required-roles) :any))))

(defn redirect-template [page]
  {:status 302
   :headers {"Location" (cpath page)}
   :body ""})

(defn redirect-to-auth [uri-prefix]
  (redirect-template (str uri-prefix "/login")))

(defn redirect-to-permission-denied [uri-prefix]
  (redirect-template (str uri-prefix "/permission-denied")))

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

(defn secure-user
  "Ensure that the user has a salt value associated with it and that if the
   password has changed, it is hashed."
  [new-user old-user]
  (let [password (:password new-user)
        salt (if-let [s (:salt old-user)]
               s
               (random-string 12 12))]
    (if (not (= (:password old-user) password))
      (-> new-user
         (assoc :salt salt)
         (assoc :password (hash-password password salt)))
      new-user)))

;;
;; Edit Users
;; ==========
;;

(def user-table-columns
     [{:column :last_name :actions #{:sort}}
      {:column :first_name :actions #{:sort}}
      {:column :username :actions #{:sort}}
      {:column :email :actions #{:sort}}
      :empty])

(defn user-list-page [props load-fn request]
  [:div
   [:div (clink-to "new" "Add new User")]
   (filter-and-sort-table
     request
     {:type :app_user :name :user-table :props props}
     user-table-columns 
     (fn [k row-data]
       (cond (= k :empty)
             [:div
              (clink-to (str "edit?id=" (:id row-data)) "Edit") ", "
              (clink-to (str "delete?id=" (:id row-data)) "Delete")]
             :else (k row-data)))
     load-fn)])

(defn user-form-fields [load-fn props]
  [(form-textfield props :username :required)
   (form-password props :new_password :required)
   (form-textfield props :first_name :required)
   (form-textfield props :last_name :required)
   (form-textfield props :email :required)
   (form-checkbox props :account_enabled)
   (form-multi-checkbox props :roles (load-fn :role) :name)])

(defn edit-user-form [data-fns props request]
  (let [lookup-fn (fn [r] ((data-fns :lookup) :app_user (:id (:params r))))
        action (if (.endsWith (:uri request) "new") :new :edit)
        form-data (if (= action :new) {} (lookup-fn request))
        title (if (= action :new) "Create New User" "Edit User")]
       (standard-form
        title (name action) "Save"
        (form-layout-grid [1 1 2 1 1 1]
                          :user
                          (conj
                           (user-form-fields (data-fns :load) props)
                           (form-hidden :id)
                           (form-hidden :password))
                          request
                          (if (= action :edit)
                            (assoc form-data :new_password "_unchanged")
                            form-data)))))

(defn create-user-from-params [load-fn request]
  (let [params (:params request)
        user (-> (select-keys params
                              [:id :username :new_password :password
                               :first_name :last_name :email])
                 (get-yes-no-fields params #{:account_enabled})
                 (get-multi-checkbox params :roles)
                 (assoc :type :app_user)
                 (clean-form-input))
        user (if (= "_unchanged" (:new_password user))
               user
               (assoc user :password (:new_password user)))]
    user))

;; make this more consice
(def invalid-user?!
     (partial
      invalid?
      :user
      (fn [props form-data]
        (merge
         (required-field form-data :username
                         (str (property-lookup props :username)
                              " is required."))
         (required-field form-data :new_password
                         (str (property-lookup props :new_password)
                              " is required."))
         (required-field form-data :first_name
                         (str (property-lookup props :first_name)
                              " is required."))
         (required-field form-data :last_name
                         (str (property-lookup props :last_name)
                              " is required."))
         (required-field form-data :email
                         (str (property-lookup props :email)
                              " is required."))))))

(defn save-user! [data-fns props request]
  (let [save-or-update-fn (data-fns :save)
        form-data (create-user-from-params (data-fns :load) request)
        submit (-> request :params :submit)
        success "list"
        failure (cpath (:uri request))]
    (redirect-to
     (cond (form-cancelled (:params request)) success
           (invalid-user?! props form-data request) failure
           :else (do
                   (save-or-update-fn (dissoc form-data :new_password))
                   (set-flash-value! :user-message
                                     "User has been saved."
                                     request)
                   success)))))

;;
;; Login
;; =====
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
;; Security Routes
;; ===============
;;

(defn security-edit-user-routes [path-prefix layout name-fn props data-fns]
  (routes
   (GET (str path-prefix "/user/list*")
        (layout (name-fn request)
                request
                (user-list-page props (data-fns :load) request)))
   (GET (str path-prefix "/user/new*")
        (layout (name-fn request)
                request
                (edit-user-form data-fns props request)))
   (POST (str path-prefix "/user/new*")
         (save-user! data-fns props request))
   (GET (str path-prefix "/user/edit*")
        (layout (name-fn request)
                request
                (edit-user-form data-fns props request)))
   (POST (str path-prefix "/user/edit*")
         (save-user! data-fns props request))
   (GET (str path-prefix "/user/delete*")
        (layout (name-fn request)
                request
                (confirm-delete (data-fns :lookup)
                                :app_user
                                (fn [u]
                                  (str (:first_name u) " " (:last_name u)))
                                props
                                (:id params))))
   (POST (str path-prefix "/user/delete*")
         (do
           (if (not (form-cancelled params))
             ((data-fns :delete) :app_user (:id params)))
           (redirect-to "list")))))

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

(defn load-user-by [load-fn user k]
  (first
   (load-fn :app_user
            {k (user k)}
            {})))

(defn standard-save-user [user load-fn delete-fn save-fn]
  (let [all-roles (index-by :name (load-fn :role))
        new-roles (map #(all-roles %) (:roles user))
        new-role-ids (set (map :id new-roles))
        user-part (dissoc user :roles)
        current-roles (load-fn :user_role
                               {:user_id (:id user)}
                               {})
        current-role-ids (set (map :role_id current-roles))
        d-roles (filter #(not (contains? new-role-ids (:role_id %)))
                        current-roles)]
    (do
      (save-fn (secure-user user-part (load-user-by load-fn user :id)))
      (let [saved-user-id (:id (load-user-by load-fn user :username))
            a-roles (map #(hash-map :type :user_role
                                    :role_id (:id %)
                                    :user_id saved-user-id)
                         (filter #(not (contains? current-role-ids
                                                  (:id %)))
                                 new-roles))]
        (do
          (doseq [next d-roles]
            (delete-fn next))
          (doseq [next a-roles]
            (save-fn next)))))))

(defn standard-lookup-user [type id load-fn lookup-fn]
  (let [user (lookup-fn type id)
        roles (load-fn :role)
        user-role-ids
        (set (map :role_id
                  (load-fn :user_role
                           {:user_id id}
                           {})))
        user-roles (filter #(contains? user-role-ids (:id %)) roles)]
    (assoc user :roles (vec (map :name user-roles)))))

(defn standard-delete-user [type id load-fn delete-fn]
  (let [user (first (load-fn type {:id id} {}))
        roles (load-fn :user_role {:user_id (:id user)} {})]
    (doseq [next-role roles]
      (delete-fn :user_role (:id next-role)))
    (delete-fn type (:id user))))

