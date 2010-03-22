(ns tools.pseudo-primes
  ;; de.tsdh.clojure.math.primes
  ;; TODO: Only expt from math is needed, but how do I use :only with multiple
  ;; libs?
  (:use [clojure.contrib math test-is]))

(def
 #^{:doc "The chances that prime? returns true for a composite if *pseudo*
is true is (expt 4 (* -1 *pseudo-accuracy*))."}
 *pseudo-accuracy* 10)

(defn factorize-out
  "Factorizes out all x factors from n.
Examples:
  (factorize-out 10 2) ==> 5, because 2^1 * 5 = 10
  (factorize-out 90 3) ==> 10, because 3^2 * 10 = 90"
  [n x]
  (loop [acc n exp 0]
    (if (= 0 (mod acc x))
      (recur (/ acc x) (inc exp))
      (hash-map :exponent exp :rest acc))))

(defn expt-mod
  "Equivalent to (mod (expt n e) m), but faster.
http://en.wikipedia.org/wiki/Modular_exponentiation#An_efficient_method:_the_right-to-left_binary_algorithm" 
  [n e m]
  (loop [r 1, b n, e e]
    (if (= e 0)
      r
      (recur (if (odd? e)
               (mod (* r b) m)
               r)
             (mod (expt b 2) m)
             (bit-shift-right e 1)))))

(defn prime?
  "Checks if n is a prime using the Miller-Rabin pseudo-primality test.  Also
see *pseudo* and *pseudo-accuracy*."
  [n]
  (cond
    (< n 2)   false
    (= n 2)   true
    (even? n) false
    :else (let [m (factorize-out (dec n) 2)
                d (:rest m)
                s (:exponent m)]
            (loop [k 1]
              (if (> k *pseudo-accuracy*)
                true
                (let [a (+ 2 (rand-int (- n 4)))
                      x (expt-mod a d n)]
                  (if (or (= x 1) (= x (dec n)))
                    (recur (inc k))
                    (if (loop [r 1
                               x (expt-mod x 2 n)]
                          (cond
                           (or (= x 1) (>  r (dec s)))  false
                           (= x (dec n))                true
                           :else (recur (inc r) (mod (* x x) n))))
                      (recur (inc k))
                      false))))))))

(defn next-prime [n]
  "Returns the next prime greater than n."
  (cond
    (< n 2)    2
    :else (loop [n (inc n)]
            (if (prime? n)
              n
              (recur (inc n))))))

;; test stuff

(def
 #^{:private true}
 first-1000-primes
 '(   2      3      5      7     11     13     17     19     23     29 
   15485867  15485917  15485927  15485933  15485941  15485959  15485989  15485993
   15486013  15486041  15486047  15486059  15486071  15486101  15486139  15486157
   15486173  15486181  15486193  15486209  15486221  15486227  15486241  15486257
   15486259  15486277  15486281  15486283  15486287  15486347  15486421  15486433
   15486437  15486451  15486469  15486481  15486487  15486491  15486511  15486517
   15486533  15486557  15486571  15486589  15486649  15486671  15486673  15486703
   15486707  15486719  15486727  15486739  15486749  15486769  15486773  15486781
   15486791  15486803  15486827  15486833  15486857  15486869  15486871  15486883
   15486893  15486907  15486917  15486929  15486931  15486953  15486967  15486997
 
 ;; [Snipped, but the test works with all 1000 primes...]
   7841   7853   7867   7873   7877   7879   7883   7901   7907   7919))

(deftest test-prime-fns
  (loop [a (take 1000 (iterate next-prime 2)) 
         b (take 1000 first-1000-primes)]
    (when (and (empty? a) (empty? b))
      (is (= (first a) (first b)))
      (recur  (rest a) (rest b)))))