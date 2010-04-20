;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.test-quick
  (:use (clojure test)
        (hiccup core)
        (sandbar quick
                 [test :only (t)])))

(deftest test-conj-or-concat
  (is (= (conj-or-concat [:a] "ab")
         [:a "ab"]))
  (is (= (conj-or-concat [:a] [:b :c])
         [:a :b :c])))

(deftest test-parse-block
  (is (= (parse-block "[a s]")
         [:a "s"]))
  (is (= (parse-block "[a [b s]]")
         [:a [:b "s"]]))
  (is (= (parse-block "[a [b s] x]")
         [:a [:b "s"] " x"]))
  (is (= (parse-block "[a [[b s] x]")
         [:a "[[b s]" " x"]))
  (is (= (parse-block "[ a [ b s] x]")
         [:a [:b "s"] " x"])))

(deftest test-block?
  (is (true? (block? "[x s]")))
  (is (false? (block? "[x s")))
  (is (false? (block? "x s]"))))

(deftest test-pre-process-page
  (is (= (pre-process-page "Here is a simple string.")
         [:page [:p "Here is a simple string."]]))
  (is (= (pre-process-page "Here is a [[simple] string.")
         [:page [:p "Here is a " "[[simple]" " string."]]))
  (is (= (pre-process-page "Here is a [b simple] string.")
         [:page [:p "Here is a " [:b "simple"] " string."]]))
  (is (= (pre-process-page "Here is a simple string.\nNewline.")
         [:page [:p "Here is a simple string.\nNewline."]]))
  (is (= (pre-process-page "Here is a simple string.\n\nNewline.")
         [:page [:p "Here is a simple string."] [:p "Newline."]]))
  (is (= (pre-process-page "Here [i is a [b simple] string.]")
         [:page [:p "Here " [:i "is a " [:b "simple"] " string."]]]))
  (is (= (pre-process-page "Here [ i is a [ b simple] string.]")
         [:page [:p "Here " [:i "is a " [:b "simple"] " string."]]])))

(def test-env {:page page-handler
               :p paragraph-handler
               :b bold-handler
               :i italic-handler})

(deftest test-q-eval
  (is (= (q-eval test-env [:page [:b "one"]])
         [:div [:b "one"]] ))
  (is (= (html (q-eval test-env [:page [:b "one"]]))
         "<div><b>one</b></div>"))
  (is (= (q-eval test-env [:page [:b "one"] [:i "two"]])
         [:div [:b "one"] [:i "two"]] ))
  (is (= (html (q-eval test-env [:page [:b "one"] [:i "two"]]))
         "<div><b>one</b><i>two</i></div>"))
  (is (= (html (q-eval test-env [:page [:b "one " [:i "two"] " three"]]))
         "<div><b>one <i>two</i> three</b></div>"))
  (is (= (html
          (q-eval test-env
                  [:page
                   [:p "Here " [:i "is a " [:b "simple"] " string."]]]))
         "<div><p>Here <i>is a <b>simple</b> string.</i></p></div>"))
  (is (= (html
          (q-eval test-env
                  [:page [:p "Paragraph One"] [:p "Paragraph Two"]]))
         "<div><p>Paragraph One</p><p>Paragraph Two</p></div>")))
