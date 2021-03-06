;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.validation
  (:use (sandbar [core :only (property-lookup)]
                 [util :only (append-to-keyword)])))

(defmacro if-valid [validator m success failure]
  `(let [result# (~validator ~m)]
     (if (= ~m result#)
       (~success ~m)
       (let [errors# (:_validation-errors result#)]
         (~failure (dissoc ~m :_validation-errors) errors#)))))

;;
;; Heavy Macro Action
;; ==================
;; The following two functions and the macro build-validator allow you
;; to write the following validator:
;;
;; (defn login-validator []
;;   (fn [m]
;;     (let [v (-> m
;;                 (non-empty-string :username)
;;                 (non-empty-string :password))]
;;       (if (= m v)
;;         (-> m
;;             password-validator)
;;         v))))
;;
;; as
;;
;; (defn login-validator []
;;  (build-validator (non-empty-string :username)
;;                   (non-empty-string :password)
;;                   :ensure
;;                   password-validator))
;;
;; which allows you to create complex validators without having to
;; think about all of the control stuff. You may have any number of
;; :ensure keywords.
;;
;; The :ensure keyword means that everything before it must be valid,
;; if not it will return with only the current errors.
;;

(defn- group-validators [coll]
  (loop [result []
         next coll]
    (if (some #(= % :ensure) next)
      (let [v (split-with #(not (= % :ensure)) next)]
        (recur (conj result (first v)) (rest (first (rest v)))))
      (conj result next))))

(defn- build-validator-control [m- groups]
  (if (> (count groups) 1)
    (let [v- (gensym "v_")]
      (list 'clojure.core/let [v- (build-validator-control m- [(first groups)])]
            (list 'if (list 'clojure.core/= m- v-)
                  (build-validator-control m- (rest groups))
                  v-)))
    (cons 'clojure.core/-> (cons m- (first groups)))))

(defmacro build-validator [& validators]
  (let [v (group-validators validators)]
    (let [m- (gensym "m_")]
      (list 'clojure.core/fn [m-]
            (build-validator-control m- v)))))

(defn add-validation-error
  ([m msg] (add-validation-error m :form msg))
  ([m k msg]
     (assoc m :_validation-errors
         (merge-with (fn [a b] (vec (concat a b)))
                     (:_validation-errors m) {k [msg]}))))

;;
;; Validators
;; ==========
;;

(defn multi-value-validator
  "Validate multiple values in a map. args is a list of keys possibly ending
   with a message string or map. v-fn is the validation function which is a
   function of the key to validate. default-message is either a string or a
   function of the message map and key which returns a string."
  [m args v-fn default-message]
  (let [key-seq (take-while keyword? args)
        msg (last args)
        msg (if (keyword? msg) {} msg)]
    (loop [key-seq key-seq
           m m]
      (if (empty? key-seq)
        m
        (let [k (first key-seq)]
          (if (v-fn k)
            (recur (rest key-seq) m)
            (recur (rest key-seq)
                   (add-validation-error
                    m
                    k
                    (if (map? msg)
                      (if-let [custom-error-message
                               (get msg (append-to-keyword
                                         k
                                         "-validation-error"))] 
                        custom-error-message
                        (if (fn? default-message)
                          (default-message k msg)
                          default-message))
                      msg)))))))))

(defn non-empty-string [m & args]
  (multi-value-validator m args
                         (fn [k]
                           (let [value (get m k)]
                             (and (string? value)
                                  (not (empty? value)))))
                         (fn [k msg]
                           (str (property-lookup msg k) " cannot be blank!"))))

