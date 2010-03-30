;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.example.stateful-sessions
  "Simple example of working with stateful sessions."
  (:use (compojure core)
        (ring.adapter jetty)
        (ring.middleware params)
        (sandbar stateful-session)))

(defn functional-handler
  "Functional style of working with a session."
  [request]
  (println request)
  (let [counter (if-let [counter (-> request :session :counter)]
                  (+ counter 1)
                  1)]
    {:status 200
     :headers {"Content-Type" "text/html"}
     :body (str "The current value of counter is " counter)
     :session {:counter counter}}))

(defn stateful-handler
  "Stateful style of working with a session."
  []
  (let [counter (+ 1 (session-get :counter 0))]
    (do (session-put! :counter counter)
        (str "The current value of counter is " counter))))

(defroutes my-routes
  (GET "/functional*" request (functional-handler request))
  (GET "/stateful*" [] (stateful-handler))
  (ANY "*" [] "Hello World!"))

(def app (-> my-routes
             wrap-stateful-session
             wrap-params))

(run-jetty (var app) {:port 8080})

