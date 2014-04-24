(ns problem051
  #_(:refer [tools.numbers] :only (str2int digits) :as numbers)
  (:require 
   [clojure.string :only (replace split) :as str]))
;; list of 8 digit primes found here:
;; http://primes.utm.edu/curios/index.php?start=8&stop=10

(def candidates 
  (str/split (slurp "src/problem051_8_digit_prime_numbers.clj") #"\n"))
(count candidates)

(frequencies (map sort (map tools.numbers/str2int candidates)))
