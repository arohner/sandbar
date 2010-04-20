(defproject sandbar "0.2.2-SNAPSHOT"
  :description "Clojure web application libraries built on top of Ring
                and Compojure."
  :dependencies [[org.clojure/clojure "1.2.0-master-SNAPSHOT"]
                 [org.clojure/clojure-contrib "1.2.0-master-SNAPSHOT"]
                 [compojure "0.4.0-SNAPSHOT"]
                 [hiccup "0.2.3"]
                 [ring/ring-jetty-adapter "0.2.0"]
                 [sandbar/sandbar-core "0.2.2-SNAPSHOT"]
                 [sandbar/sandbar-session "0.2.2-SNAPSHOT"]
                 [sandbar/sandbar-auth "0.2.2-SNAPSHOT"]
                 [sandbar/sandbar-dev "0.0.1-SNAPSHOT"]]
  :dev-dependencies [[lein-clojars "0.5.0-SNAPSHOT"]
                     [ring/ring-devel "0.2.0"]
                     [ring/ring-httpcore-adapter "0.2.0"]
                     [ring/ring-servlet "0.2.0"]
                     [mysql/mysql-connector-java "5.1.6"]
                     [mstate "0.0.1-SNAPSHOT"]
                     [jline "0.9.94"]])
