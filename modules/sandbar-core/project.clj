(defproject sandbar/sandbar-core "0.3.0"
  :description "Core library for sandbar project."
  :dependencies [[org.clojure/clojure "1.1.0"]
                 [org.clojure/clojure-contrib "1.1.0"]
                 [hiccup "0.2.3"]]
  :dev-dependencies [[lein-clojars "0.5.0-SNAPSHOT"]
                     [jline "0.9.94"]]
  :namespaces [sandbar.test
               sandbar.util
               sandbar.core])
