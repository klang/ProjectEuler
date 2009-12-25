(ns project-euler-test
  (:use clojure.contrib.test-is))

(def project-euler-tests
     (map #(symbol (str "" (name %)))
	  [:problem006]))

(def all-tests (concat project-euler-tests))

(doseq [test all-tests] (require test))

(apply run-tests all-tests)

;;(shutdown-agents)