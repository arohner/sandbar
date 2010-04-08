;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.dev.validation
  (:use (sandbar.dev util)))

(defmacro if-valid [validator m success failure]
  `(let [result# (~validator ~m)]
     (if (= ~m result#)
       (~success ~m)
       (let [errors# (:_validation-errors result#)]
         (~failure (dissoc ~m :_validation-errors) errors#)))))

(defn add-validation-error [m k msg]
  (assoc m :_validation-errors
         (merge-with (fn [a b] (vec (concat a b)))
                     (:_validation-errors m) {k [msg]})))

;;
;; Validators
;; ==========
;;

(defn non-empty-string [m k msg]
  (let [value (get m k)]
    (if (and (string? value)
             (not (empty? value)))
      m
      (add-validation-error
       m
       k
       (if (map? msg)
         (str "The " (property-lookup msg k) " field cannot be blank?")
         msg)))))

