; Copyright (c) Brenton Ashworth. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file COPYING at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns sandbar.test
  (:use [clojure.test]
        [clojure.test :only (run-tests)]
        [sandbar.util]))
 
(def test-dir "test")
(def test-file-prefix "test_")
(def test-file-ext ".clj")
 
(def *logging* false)
 
(defn set-logging! [b]
  (def *logging* b))
 
(defmacro def-test [name & body]
  (if *logging*
    `(deftest ~name
       (println (str ~name))
       (time (do ~@body)))
    `(deftest ~name ~@body)))

(defmacro t [desc & body] `(testing ~desc ~@body))
 
(defn- is-test-file? [file-info]
  (let [file-name (first file-info)
        re-s (str test-file-prefix ".*" test-file-ext)
        re-p (re-pattern re-s)]
    (re-matches re-p file-name)))
 
(defn- get-file-info [f]
  [(.getName f) (file-to-ns-string f test-dir)])
 
(def test-namespaces
  (map #(symbol (last %))
    (file-seq-map-filter test-dir get-file-info is-test-file?)))
 
(defn namespace-filter? [arg]
  (if (or (nil? arg) (.startsWith arg "-")) false true))
 
(defn get-test-namespaces []
  (let [arg (first *command-line-args*)]
    (if (namespace-filter? arg)
        (filter #(.endsWith (name %) arg) test-namespaces)
        test-namespaces)))
 
(defn run-sequential? []
  (if (= (first *command-line-args*) "-seq") true false))
 
(defn run
  "Runs all defined tests"
  []
  (println "\nLoading tests...")
  (let [namespaces (get-test-namespaces)]
    (apply require :reload-all namespaces)
    (if (run-sequential?)
        (time (apply run-tests namespaces))
        (let [agents (map #(agent %) namespaces)]
          (println "Running tests with" (count agents) "agents")
          (time
            (do
              (dorun
               (map #(send-off %
                               (fn [x]
                                 (with-out-str (time (run-tests x))))) agents))
              (apply await agents)
              (dorun (map #(println "\nAgent report:\n" (deref %)) agents))))))
    (shutdown-agents)))
