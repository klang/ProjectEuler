(ns hard-primes
  (meta {:description "working from a set of zipped lists of hardcoded primes is sometimes faster than generating them on the fly."})
  (:use [clojure.contrib.str-utils2 :only (split)]))


(defn str2int [str] (. Integer parseInt str 10))
(defn blanks [str] (= "" str))

(defn read-data [filename]
  (map str2int 
       (remove blanks (reduce into [] (drop 2 (map #(split % #" +") (split (slurp filename) #"\r\r\n")))))))

;(time (def p1 (read-data "src/tools/primes1.txt")))
;;"Elapsed time: 5673.801488 msecs"
;;#'hard-primes/p1
;;(last p1) is 15485863

;(use '[tools.primes :only (primes-up-to)])

;;hard-primes>  (time (def p1 (primes-up-to 15485863)))
;;"Elapsed time: 4254.095807 msecs"
;;#'hard-primes/p1
