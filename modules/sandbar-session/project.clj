(defproject sandbar/sandbar-session "0.2.3"
  :description "Easier session management for Ring."
  :dependencies [[org.clojure/clojure "1.1.0"]
                 [org.clojure/clojure-contrib "1.1.0"]
                 [ring/ring-core "0.2.3"]
                 [sandbar/sandbar-core "0.3.0"]]
  :dev-dependencies [[lein-clojars "0.5.0-SNAPSHOT"]
                     [jline "0.9.94"]]
  :namespaces [sandbar.stateful-session])
