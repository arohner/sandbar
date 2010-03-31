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

(def *session* (atom nil))

(defn wrap-stateful-session
  "Create a binding for *session* which is an atom containing the session
   map. The contents of this atom will only be put in the response if the
   response does not already contain :session."
  [handler]
  (wrap-session
   (fn [request]
      (binding [*session* (atom (:session request))]
        (let [response (handler request)]
          (if (:session response)
            response
            (merge response {:session @*session*})))))))

(defn update-session! [update-fn value]
  (swap! *session* update-fn value))

(defn session-put! [k v]
  (swap! *session* (fn [a b] (merge a {k b})) v))

(defn session-get
  ([k] (session-get k nil))
  ([k default] (if (vector? k)
                 (get-in @*session* k)
                 (get @*session* k default))))

(defn session-delete-key! [k]
  (swap! *session* (fn [a b] (dissoc a b)) k))

(defn set-flash-value! [k v]
  (session-put! k v))

(defn get-flash-value! [k]
  (let [v (get @*session* k)]
    (do (session-delete-key! k)
        v)))
