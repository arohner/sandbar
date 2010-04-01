;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.quick
  "Quick is English for Wiki."
  (:require [clojure.contrib.str-utils2 :as s]))

(declare pre-process-string)

(defn conj-or-concat [v s]
  (vec (if (string? s)
         (conj v s)
         (concat v s))))

(defn parse-block [s]
  (let [string-as-data (read-string s)
        fn-key (keyword (first string-as-data))
        attrs (second string-as-data)
        inner-string (pre-process-string
                      (apply str
                             (interpose " "
                                        (drop (if (map? attrs) 2 1)
                                              string-as-data))))]
    (vec (filter #(not (and (string? %)
                            (empty? %)))
                 (if (map? attrs)
                   (conj-or-concat [fn-key attrs] inner-string)
                   (conj-or-concat [fn-key] inner-string))))))

(defn block? [s]
  (and (.startsWith s "[")
       (not (.startsWith s "[["))
       (.endsWith s "]")))

(defn pre-process-string [s]
  (if (re-matches #".*\[.*\].*" s)
    (map #(if (block? %)
            (parse-block %)
            %)
         (s/partition s #"\[.*\]"))
    s))

(defn pre-process-page [page]
  (vec
   (concat [:page]
         (map #(let [processed (pre-process-string %)]
                 (conj-or-concat [:p] processed))
              (s/split page #"\n\n")))))

(declare q-eval)

(defn q-apply [handler env coll]
  (let [k (first coll)
        attrs (second coll)
        body (drop (if (map? attrs) 2 1) coll)]
    (handler env
             (fn [env]
               (map #(if (string? %)
                       %
                       (q-eval env %)) body)))))

(defn q-eval [env form]
  (let [k (first form)
        handler (k env)]
    (if handler
      (q-apply handler env form))))

;; Example handlers

(defn page-handler [env apply-fn]
  (vec (concat [:div] (apply-fn env))))

(defn bold-handler [env apply-fn]
  (vec (concat [:b] (apply-fn env))))

(defn italic-handler [env apply-fn]
  (vec (concat [:i] (apply-fn env))))

(defn paragraph-handler [env apply-fn]
  (vec (concat [:p] (apply-fn env))))
