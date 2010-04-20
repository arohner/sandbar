;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.util
  (:use [clojure.contrib.str-utils :only (re-split re-gsub)])
  (:import (java.io File)))

;;
;; Misc
;; ====
;;

(defn append-to-keyword [k s]
  (keyword (str (name k) s)))

(defn to-keywords [coll]
  (map #(keyword %) coll))

(defn path-to-seq [path]
  (filter #(not (.equals % "")) (re-split #"/" path)))

(defn seq-to-path [coll]
  (apply str (interleave (repeat "/") coll)))

(defn random-between [lo hi]
  (let [r (java.util.Random.)
        lo (if (char? lo) (int lo) lo)
        hi (if (char? hi) (int hi) hi)
        n (+ 1 (- hi lo))]
    (+ lo (Math/abs (mod (. r nextInt) n)))))

(defn random-string [lo hi]
  (loop [length (random-between lo hi)
         v []]
    (if (> length 0)
      (let [j (random-between 1 3)]
        (recur (dec length)
               (conj v
                     (cond (= j 1) (char (random-between \a \z))
                           (= j 2) (char (random-between \A \Z))
                           (= j 3) (char (random-between \1 \9))))))
      (apply str v))))

(defn index-by [k coll]
  (reduce (fn [a b]
            (let [v (k b)]
              (assoc a v b)))
          {}
          coll))

;;
;; Files
;; =====
;;

(defn without-ext [s]
  (apply str (interpose "." (reverse (rest (reverse (re-split #"[.]" s)))))))

(defn remove-file-ext [file-name]
  (let [index (.lastIndexOf file-name ".")]
    (if (> index -1)
      (apply str (first (split-at index file-name)))
      file-name)))
 
(defn file-to-ns-string [f root-dir]
  (let [f-sep File/separator
        test-dir-pattern (re-pattern (str f-sep root-dir f-sep))]
    (re-gsub (re-pattern f-sep) "."
             (remove-file-ext
              (last (re-split test-dir-pattern (.getAbsolutePath f)))))))
 
(defn file-seq-map-filter [dir mf ff]
  (filter ff (map mf (file-seq (File. dir)))))

;;
;; Formats
;; =======
;;

(defn format-csv [coll-of-rows]
  (let [data (map #(interpose "," %) coll-of-rows)]
    (apply str
           (map #(if (nil? %) "\"\""  (if (and (not (= % "\n"))
                                               (not (= % ",")))
                                        (str "\"" % "\"")
                                        %))
                           (reduce (fn [a b] (concat a b ["\n"])) [] data)))))

(defn date-string
  ([] (date-string (java.util.Date.)))
  ([d]
     (let [formatter (java.text.SimpleDateFormat. "yyyy-MM-dd")]
       (.format formatter d))))
