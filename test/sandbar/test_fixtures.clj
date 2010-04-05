;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.test-fixtures)

(def fixture-security-config
     [#"/admin.*"                 [:admin :ssl] 
      #"/idea/edit.*"             :editor
      #"/download.*"              [#{:admin :editor} :ssl]
      #"/secure.*"                :ssl
      #"/login.*"                 [:any :ssl] 
      #"/permission-denied.*"     :any
      #".*.css|.*.png"            [:any :any-channel] 
      #(.endsWith (:uri %) ".js") :any
      #".*"                       [#{:admin :user} :nossl]])

