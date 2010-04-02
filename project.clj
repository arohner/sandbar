(defproject sandbar "0.1.0-SNAPSHOT"
  :description "Clojure web application library."
  :dependencies [[org.clojure/clojure "1.2.0-master-SNAPSHOT"]
                 [org.clojure/clojure-contrib "1.2.0-master-SNAPSHOT"]
                 [compojure "0.4.0-SNAPSHOT"]
                 [hiccup "0.2.1"]
                 [ring/ring-devel "0.2.0"]
                 [ring/ring-httpcore-adapter "0.2.0"]
                 [ring/ring-jetty-adapter "0.2.0"]
                 [ring/ring-servlet "0.2.0"]
                 [mysql/mysql-connector-java "5.1.6"]]
  :namespaces [sandbar.database
               sandbar.library
               sandbar.auth
               sandbar.userui])
