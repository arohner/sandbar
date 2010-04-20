;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.core
  (:use (hiccup core page-helpers)))

(def app-context (atom ""))

(defn set-app-context! [context]
  (swap! app-context (fn [a b] b) context))

(defn cpath [path]
  (if (.startsWith path "/")
    (str @app-context path)
    path))

(defn remove-cpath [path]
  (let [c @app-context]
    (if (not (empty? c))
      (apply str (drop (count c) path))
      path)))

(defn clink-to [path title]
  (link-to (cpath path) title))

(defn redirect-301 [url]
  {:status 301
   :headers {"Location" (cpath url)}})

(defn redirect? [m]
  (or (= (:status m) 302)
      (= (:status m) 301)))

(defn append-to-redirect-loc
  "Append the uri-prefix to the value of Location in the headers of the
   redirect map."
  [m uri-prefix]
  (if (or (nil? uri-prefix) (empty? uri-prefix))
    m
    (let [loc (remove-cpath ((:headers m) "Location"))]
      (if (re-matches #".*://.*" loc)
        m
        (merge m {:headers {"Location" (cpath (str uri-prefix loc))}})))))

(defn property-lookup [p k]
  (k p (name k)))

;; Resource locations under /public
(def css-path "/css/")
(def image-path "/images/")
(def js-path "/js/")

(defn stylesheet [name]
  (include-css (cpath (str css-path name))))

(defn javascript [name]
  (include-js (cpath (str js-path name))))

(defn icon [name]
  [:link {:rel "icon" :type "image/png" :href (cpath (str image-path name))}])

(defn image
  ([name] (image name {:alt name}))
  ([name attrs] 
    [:img (merge {:src (cpath (str image-path name)) :border "0"} attrs)]) 
  ([name mouseover attrs]
     (let [c-path (cpath image-path)]
       [:img (merge {:src (str c-path name) :border "0" 
           :onmouseout (str "this.src='" c-path name "'") 
           :onmouseover (str "this.src='" c-path mouseover "'")} attrs)])))

(defn image-link
  ([path name] (image-link path name {:alt name})) 
  ([path name attrs]
    (link-to (str (cpath path)) (image name attrs)))
  ([path name mouseover attrs]
     (link-to (str (cpath path)) (image name mouseover attrs))))

