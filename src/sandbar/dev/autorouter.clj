;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.dev.autorouter
  "Rails style routing."
  (:use (compojure core)
        (hiccup core)
        (clojure.contrib [str-utils :only (re-split)]
                         [ns-utils :only (ns-vars)])))

(defn get-controller-ns
  "Given a root-ns, generate the name of the controller namespace. Controller
   namespaces are located relative to the root-ns on the classpath."
  ([root-ns] (get-controller-ns root-ns "control"))
  ([root-ns controller]
     (let [parts (re-split #"[.]" (str root-ns))
           ctrl (str "." controller)]
       (str (apply str
                   (interpose "." (take (- (count parts) 1) parts)))
            ctrl))))

(defn load-namespace
  "Require a namespace and return true if it is found. Return false if not."
  [ns-name]
  (try
   (do
     (require :reload (symbol ns-name))
     true)
   (catch java.io.FileNotFoundException e false)))

(defn namespace-and-action-exist?
  "Return true if the namespace exists and contains a var with same name as
   the passed action."
  [ns-name action]
  (if (and
       (load-namespace ns-name)
       (some #(= % (symbol action))
             (ns-vars (symbol ns-name))))
    ns-name
    false))

(defn action-exists?
  "Return true if an action is located. If the controller is nil then look
   for the action in the default controller namespace."
  [root-ns controller action]
  (cond (and (not controller) action)
        (namespace-and-action-exist? (get-controller-ns root-ns)
                                     action)
        (and controller action)
        (namespace-and-action-exist? (get-controller-ns root-ns controller)
                                     action)
        :else false))

(defn uri-seq [request]
  (filter #(not (empty? %)) (re-split #"/" (:uri request))))

(defn query-part [request controller action]
  (let [path (uri-seq request)]
    (cond (and controller action) (drop 2 path)
          (or controller action) (drop 1 path)
          :else "")))

;; TODO 1) Configure a path prefix
;;      2) Maybe add a thread local var which contains path
;;         information so that you can create functions that can
;;         easily link to the same controller and other controllers.
;;      3) Configure if :reload should be used
;;      4) How do we deal with layouts
(defn autorouter
  "Dynamically find an action based on the URI. The action must be a function
   of the request. For example, the URI /edit will look for a function named
   edit in the namespace <ns-prefix>.controller. The URI /user/edit will look
   for the edit function in the namespace <ns-prefix>.controller.user. If
   your routes are defined in com.company.routes then <ns-prefix> =
   com.company. This function will automatically deal with HEAD and TRACE
   requests (not implemented), you must provide you own implementaions for
   GET, POST, PUT and DELETE. If a GET request is /user/edit then it will look
   for a function named edit. If another request method is user it will look
   for edit-post, edit-put, edit-delete."
  [route-adapter]
  (let [ns *ns*]
    (routes
     (ANY "*" request
          (let [path (uri-seq request)
                request-method (:request-method request)
                action-suffix (if (= request-method :get)
                                ""
                                (str "-" (name request-method)))]
            (loop [controller nil
                   action (if-let [a (first path)] a "index")
                   params (rest path)]
              (let [[ctrl act] (route-adapter controller action)
                    act-meth (str act action-suffix)]
                (if-let [ctrl-ns (action-exists? ns ctrl act-meth)] 
                  (let [ctrl-fn (ns-resolve (symbol ctrl-ns)
                                            (symbol act-meth))]
                    (ctrl-fn (merge request {:? (query-part request
                                                            controller
                                                            action)})))
                  (if controller
                    nil
                    (recur action (first params) (rest params)))))))))))
