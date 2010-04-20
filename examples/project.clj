(defproject sandbar/examples "0.2.3"
  :description "Example code for the sandbar project. Includes the ideadb
                reference application."
  :dependencies [[org.clojure/clojure "1.1.0"]
                 [org.clojure/clojure-contrib "1.1.0"]
                 [compojure "0.4.0-SNAPSHOT"]
                 [hiccup "0.2.3"]
                 [ring/ring-jetty-adapter "0.2.0"]
                 [sandbar/sandbar-core "0.3.0"]
                 [sandbar/sandbar-session "0.2.3"]
                 [sandbar/sandbar-auth "0.2.3"]
                 [sandbar/sandbar-dev "0.0.1-SNAPSHOT"]]
  :dev-dependencies [[ring/ring-devel "0.2.0"]
                     [ring/ring-httpcore-adapter "0.2.0"]
                     [ring/ring-servlet "0.2.0"]
                     [mysql/mysql-connector-java "5.1.6"]
                     [mstate "0.0.1-SNAPSHOT"]
                     [jline "0.9.94"]])
