; Copyright (c) Brenton Ashworth. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file COPYING at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns sandbar.dev.util
  (:use [clojure.contrib.str-utils :only (re-split re-gsub)])
  (:import (java.io File)))
 
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
