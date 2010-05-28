(defproject sandbar/sandbar-dev "0.0.1-SNAPSHOT"
  :description "New sandbar libraries which are not ready to be unleashed."
  :dependencies [[org.clojure/clojure "1.1.0"]
                 [org.clojure/clojure-contrib "1.1.0"]
                 [compojure "0.4.0-SNAPSHOT"]
                 [hiccup "0.2.3"]
                 [sandbar/sandbar-core "0.3.0"]
                 [sandbar/sandbar-session "0.2.3"]
                 [sandbar/sandbar-auth "0.2.3"]
                 [inflections "0.3"]]
  :dev-dependencies [[lein-clojars "0.5.0-SNAPSHOT"]
                     [jline "0.9.94"]
                     [mysql/mysql-connector-java "5.1.6"]]
  :namespaces [sandbar.autorouter
               sandbar.basic-authentication
               sandbar.database
               sandbar.forms
               sandbar.list-manager
               sandbar.standard-pages
               sandbar.tables
               sandbar.stats
               sandbar.user-manager
               sandbar.validation])
