(ns problem132
  #^{:description "A number consisting entirely of ones is called a repunit. We shall define R(k) to be a repunit of length k.

For example, R(10) = 1111111111 = 11412719091, and the sum of these prime factors is 9414.

Find the sum of the first forty prime factors of R(109)."
     :tests {11 [11], 111 [3 37], 1111 [11 101], 11111 [41 271], 111111 [3 7 11 13 37], 1111111 [239 4649], 11111111 [11 73 101 137], 111111111 [3 3 37 333667], 1111111111 [11 41 271 9091], 11111111111 [21649 513239], 111111111111 [3 7 11 13 37 101 9901], 1111111111111 [53 79 265371653], 11111111111111 [11 239 4649 909091]}
     }
  (:use tools.primes))
