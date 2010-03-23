(defproject sandbar "0.1.0-SNAPSHOT"
  :description "Clojure web application library."
  :dependencies [[org.clojure/clojure "1.1.0"]
                 [org.clojure/clojure-contrib "1.1.0-master-SNAPSHOT"]
                 [compojure "0.3.2"]
                 [mysql/mysql-connector-java "5.1.6"]]
  :namespaces [sandbar.database
               sandbar.library
               sandbar.security])
