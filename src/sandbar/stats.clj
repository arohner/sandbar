; Copyright (c) Brenton Ashworth. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns sandbar.stats
  (:use [clojure.contrib.duck-streams :only (reader)])
  (:import java.io.File))

(defn non-blank? [line] (if (re-find #"\S" line) true false))

(defn non-source? [file] (and
                       (not (.contains (.toString file) ".svn"))
                       (not (.contains (.toString file) "#"))))

(defn does-not-contain? [ext file-path]
  (every? false? (map #(.contains file-path %) ["prototype" "lightbox"])))

(defn source-fn [ext] 
  (fn [file]
    (let [file-string (.toString file)] 
      (and (does-not-contain? ext file-string) (.endsWith file-string ext)))))

(defn loc [base-file ext]
  (reduce
    +
    (for [file (file-seq base-file)
           :when (and ((source-fn ext) file) (non-source? file))]
      (with-open [rdr (reader file)]
        (count (filter non-blank? (line-seq rdr)))))))

(defn print-stats []
  (println "Lines of Code")
  (println "src Clojure:   " (loc (File. "src") ".clj"))
  (println "src Javascript:" (loc (File. "public") ".js"))
  (println "src CSS:       " (loc (File. "public") ".css"))
  (println "test Clojure:  " (loc (File. "test")  ".clj")))

