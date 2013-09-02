{ '[clojure.contrib.math :only (expt lcm)]    '[clojure.math.numeric-tower :only (expt lcm)]
  '[clojure.contrib.lazy-seqs :only (primes)] '[tools.primes :only (primes)]
  '[clojure.contrib.combinatorics]            '[clojure.math.combinatorics]
  '[clojure.contrib.str-utils2 :only (split)] '[clojure.string :only (split)]
}

{'[clojure.math.combinatorics] :comb
 '[clojure.math.numeric-tower] :math
 '[clojure.string]             :str
 }


'[org.clojure/math.numeric-tower "0.0.2"]

:next problem009.clj

[clojure.test :only (deftest is)]

[tools.primes :only (divisors factors) :as tools]
[tools.numbers :only (fibos digits indexed)]
[clojure.math.combinatorics :only (subsets) :as comb]
[clojure.math.numeric-tower :only (expt lcm) :as math]

(:use 
 [clojure.test :only (deftest is)]
 [tools.primes :only (prime? primes)]
 [clojure.math.combinatorics :only ()]
 [clojure.math.numeric-tower :only ()])

[clojure.string :only (split) :as str]

  (:use 
   [clojure.test :only (deftest is)]
   [tools.primes :only (primes)]
   [tools.numbers :only (digit-set)])
  (:require
   [clojure.string :only (split) :as str]
   [clojure.set :only (union) :as set]
   [clojure.math.combinatorics :only (combinations) :as comb])
