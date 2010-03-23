(defproject sandbar "0.0.1-SNAPSHOT"
  :description "Clojure web application library."
  :dependencies [[org.clojure/clojure "1.2.0-master-SNAPSHOT"]
                 [org.clojure/clojure-contrib "1.2.0-master-SNAPSHOT"]
                 [compojure "0.3.2"]
                 [mysql/mysql-connector-java "5.1.6"]]
  :namespaces [sandbar.database
               sandbar.library
               sandbar.auth
               sandbar.userui])
