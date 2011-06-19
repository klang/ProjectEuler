(defproject ProjectEuler "0.3"
  :description "Problems solved on http://projecteuler.net"
  :url "http://github.com/klang/ProjectEuler"
  :source-path "src"
  ;;:repl-init-script "src/all.clj"
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
		 [incanter "1.2.3"]]
  :dev-dependencies [[swank-clojure "1.2.1"]]
  :jvm-opts ["-Xmx512M"])

