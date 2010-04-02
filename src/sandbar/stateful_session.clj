;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.stateful-session
  "Middleware for working with 'stateful' sessions."
  (:use (ring.middleware session)))

(declare *sandbar-session*)

(defn wrap-stateful-session*
  "Add stateful sessions to a ring handler. Does not modify the functional
   behavior of ring sessions except that returning nil will not remove
   the session if you have stateful data. Creates a separate namespace
   for stateful session keys so that user code and library code will not
   interfere with one another."
  [handler]
  (fn [request]
    (binding [*sandbar-session* (atom
                                 (-> request :session :sandbar-session))]
        (let [response (handler request)
              sandbar-session @*sandbar-session*
              sandbar-session (if (empty? sandbar-session)
                                nil
                                sandbar-session)
              request-session (dissoc (:session request) :sandbar-session)
              response-session (:session response)
              session  (if (contains? response :session)
                         (or response-session {})
                         request-session) 
              session (if sandbar-session
                        (assoc session :sandbar-session sandbar-session)
                        session)]
          (if (not (or (nil? session)
                       (empty? session)))
            (merge response {:session session})
            (dissoc response :session))))))

(defn wrap-stateful-session [handler]
  (wrap-session
   (wrap-stateful-session* handler)))

(defn update-session! [update-fn value]
  (swap! *sandbar-session* update-fn value))

(defn session-put! [k v]
  (swap! *sandbar-session* (fn [a b] (merge a {k b})) v))

(defn session-get
  ([k] (session-get k nil))
  ([k default] (if (vector? k)
                 (get-in @*sandbar-session* k)
                 (get @*sandbar-session* k default))))

(defn session-delete-key! [k]
  (swap! *sandbar-session* (fn [a b] (dissoc a b)) k))

(defn set-flash-value! [k v]
  (session-put! k v))

(defn get-flash-value! [k]
  (let [v (get @*sandbar-session* k)]
    (do (session-delete-key! k)
        v)))
