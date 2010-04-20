(defproject sandbar "0.2.3"
  :description "Clojure web application libraries built on top of Ring
                and Compojure."
  :dependencies [[org.clojure/clojure "1.1.0"]
                 [org.clojure/clojure-contrib "1.1.0"]
                 [ring/ring-jetty-adapter "0.2.0"]
                 [sandbar/sandbar-core "0.3.0"]
                 [sandbar/sandbar-session "0.2.3"]
                 [sandbar/sandbar-auth "0.2.3"]
                 [sandbar/sandbar-dev "0.0.1-SNAPSHOT"]]
  :dev-dependencies [[lein-clojars "0.5.0-SNAPSHOT"]
                     [jline "0.9.94"]])
