;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.test
  (:use (clojure test)))
 
(def logging false)
 
(defn set-logging! [b]
  (def logging b))
 
(defmacro def-test [name & body]
  (if logging
    `(deftest ~name
       (println (str ~name))
       (time (do ~@body)))
    `(deftest ~name ~@body)))

(defmacro t [desc & body] `(testing ~desc ~@body))
