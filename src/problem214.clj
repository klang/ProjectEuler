(ns problem214
  (meta {:description "Let φ be Euler's totient function, i.e. for a natural number n, φ(n) is the number of k, 1 <= k <= n, for which gcd(k,n) = 1.

By iterating φ, each positive integer generates a decreasing chain of numbers ending in 1.
E.g. if we start with 5 the sequence 5,4,2,1 is generated.
Here is a listing of all chains with length 4:

[5 4 2 1] [7 6 2 1] [8 4 2 1] [9 6 2 1] [10 4 2 1] [12 4 2 1] [14 6 2 1] [18 6 2 1]
 p         p

Only two of these chains start with a prime, their sum is 12.

What is the sum of all primes less than 40000000 which generate a chain of length 25?"})
  (:use :reload-all tools.primes)
  (:use :reload-all problem072)
  (:use clojure.test)
  (:use [clojure.contrib.lazy-seqs :only (primes)]))

(defn chain [n]
  "naïve calculation of the totient chain (does a lot of recalculation)"
  (lazy-cat (take-while #(< 1 %) (iterate totient n)) [1]))

(defn chain-length [n]
  (count (chain n)))

;; (take 23 (iterate inc 1))
;; (take 23 totient-seq)
;; (take 23 (map #(chain-length %) (iterate inc 1)))
;; (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23) ; i
;; (0 1 1 2 2 4 2 6 4 6  4 10  4 12  6  8  8 16  6 18  8 12 10 22) ; totients
;; (0 1 2 3 3 4 3 4 4 4  4  5  4  5  4  5  5  6  4  5  5  5  5  6) ; chain-length
;;  0 ...     1 + phi[(phi[i]]    

;; The chain-length array can be build up from as we calculate the totients.
;; When searching for the next prime, each index that we pass has to be changed
;; to 1 + phi[phi[i]]. phi[i] will always be less than i, and if phi[i] has 
;; already been modified, it holds the length of a chain starting in phi[i]
;; .. so, instead of inserting the totient in position i, we lookup and add one.

(def phi (lazy-cat (list 0) (take 23 totient-seq)))
(def len [0 1 2])
;(defn chain-length [i phi]  (+ 1 (nth phi (nth len i))))

(defn compare-data [n] (map #(vector %1 %2 (chain-length %1) (chain %1)) (iterate inc 1) (drop 1 (make-tots-seq4 n))))

;; initializing the array with (iterate inc 0) takes half a second for 1e6 elements, 
;; for 4e7, 33 seconds (more than half of the allocated time, which might be too much
;; Not initializing the array with (iterate inc 0) means we can't use an int-array
;; as the intermediate calculations would have to hold the ratios, 
;; until they would be muliplied with i

(comment 
  ;; manual run of make-chain-seq before extra optimations were made
  (def limit 10)
  (def tots (int-array limit (iterate inc 0)))
  ;;loop
  (do (def i (int 0)) (def p (int 2)))
  (do (<= limit p) (<= limit i)
      (aset tots i (int (*  (aget tots i) (/ (- p 1) p)))))  ; v[0] = 0
  (take 10 tots)
  ;;recur :else
  (do (def i (+ i p)) (def p p) (<= limit p) (<= limit i)
      (aset tots i (int (*  (aget tots i) (/ (- p 1) p)))))  ; v[2] = 1
  (take 10 tots)
  ;;recur :else
  (do (def i (+ i p)) (def p p) (<= limit p) (<= limit i)
      (aset tots i (int (*  (aget tots i) (/ (- p 1) p)))))  ; v[4] = 2
    (take 10 tots)
  (do (def i (+ i p)) (def p p) (<= limit p) (<= limit i)
      (aset tots i (int (*  (aget tots i) (/ (- p 1) p)))))  ; v[6] = 3
  (take 10 tots)
  (do (def i (+ i p)) (def p p) (<= limit p) (<= limit i)
      (aset tots i (int (*  (aget tots i) (/ (- p 1) p)))))  ; v[8] = 4  
  (take 10 tots)
  (do (def i (+ i p)) (def p p) (<= limit p) (<= limit i)    ; i = 10
      (= '(0 1 1 3 2 5 3 7 4 9) (take 10 tots))  
      (def p (update-with-length-up-to-prime p tots))
      (= '(0 1 2 3 2 5 3 7 4 9) (take 10 tots))
      (def i 0)) 
  (take 10 tots)
  (do (aset tots i (int (*  (aget tots i) (/ (- p 1) p)))) 
    (def i (+ i p)) (def p p) (<= limit p) (<= limit i) )  ; v[0] = 0  
  ; i=p in update-with-length-up-to-prime recur, would avoid setting v[0]=0
  (do (aset tots i (int (*  (aget tots i) (/ (- p 1) p)))) 
    (def i (+ i p)) (def p p) (<= limit p) (<= limit i) )  ; v[3] = 2
  ; i=2*p and v[p]=p-1 in recur, to avoid this step too!
  (take 10 tots)
  (do (aset tots i (int (*  (aget tots i) (/ (- p 1) p)))) 
    (def i (+ i p)) (def p p) (<= limit p) (<= limit i) )   ;v[6] = 2
  (take 10 tots)
  (do (aset tots i (int (*  (aget tots i) (/ (- p 1) p)))) 
    (def i (+ i p)) (def p p) (<= limit p) (<= limit i)
      (= '(0 1 2 2 2 5 2 7 4 6) (take 10 tots))  
      (def p (update-with-length-up-to-prime p tots))
      (= '(0 1 2 3 3 5 2 7 4 6) (take 10 tots))
      (def i 0))
  (do (aset tots i (int (*  (aget tots i) (/ (- p 1) p)))) 
    (def i (+ i p)) (def p p) (<= limit p) (<= limit i) )  ; v[0] = 0  
  (do (aset tots i (int (*  (aget tots i) (/ (- p 1) p)))) 
    (def i (+ i p)) (def p p) (<= limit p) (<= limit i) )  ; v[5] = 4
  (do 
      (= '(0 1 2 3 3 4 2 7 4 6) (take 10 tots))  
      (def p (update-with-length-up-to-prime p tots))
      (= '(0 1 2 3 3 4 3 7 4 6) (take 10 tots))
      (def i 0))
  (do (aset tots i (int (*  (aget tots i) (/ (- p 1) p)))) 
    (def i (+ i p)) (def p p) (<= limit p) (<= limit i) )  ; v[0] = 0  
  (do (aset tots i (int (*  (aget tots i) (/ (- p 1) p)))) 
      (def i (+ i p)) (def p p) (<= limit p) (<= limit i)   ; v[5] = 4
      (= '(0 1 2 3 3 4 3 6 4 6) (take 10 tots))  
      (def p (update-with-length-up-to-prime p tots))
      (= '(0 1 2 3 3 4 3 4 4 4) (take 10 tots))
      (def i 0))
  (<= limit p)
  tots
  (= (take 10 (drop 1 tots))
     (map #(chain-length %) (range 1 10))
     '(1 2 3 3 4 3 4 4 4))
)

(defn update-with-length-up-to-prime [p coll]
  "this function is not free of side effects. coll is changed. 
int-arrays not immutable! but they are very fast"
  (loop [p (int p)] 
    (if (< p (count coll)) 
      (if (= p (aget coll p)) p 
	  (do (aset coll p (+ 1 (aget coll (int (aget coll p))))) ;; v[i] = 1 + v[v[i]] 
	      (recur (inc p))))
      (+ (count coll) 1))))

;; same function as problem072/make-tots-seq6
;; with search-for-index-from-i replaced by update-with-length-up-to-prime
(defn make-chains [limit]
  (loop [i (int 0), p (int 2), calc (/ (- p 1) p), 
	 chains (int-array limit (iterate inc 0))]
    (if (<= limit p)
      chains
      (if (<= limit i) 
	(let [p (int (update-with-length-up-to-prime p chains))] 
	  (recur (int (* 2 p)) p (/ (- p 1) p) 
		 (if (<= limit p) 
		   chains
		   (do (aset chains p (int (- p 1))) chains))))
	(recur (+ i p) p calc 
	       (do (aset chains i (int (* (aget chains i) calc))) chains))))))

(deftest test-make-chains
  (is (= (take 100 (drop 1 (make-chains 100)))
	 (map #(chain-length %) (range 1 100)))))

;; we have to generate everything anyway, so we might as well stop putting more checks
;; into make-chains
;; instead, we will make a function that returns the interesting values

(defn find-specific-lengths [length chains]
  (loop [i (int 0) sum 0]
    (if (<= (count chains) i)
      sum
      (recur (inc i) (if (= length (aget chains i)) (+ sum i) sum)))))

;; but then we have to generate the primes again .. 

;;; --------------------------- catch sum
(set! *warn-on-reflection* true)
(defn chain-sum [length limit]
  (loop [i (int 0), p (int 2), calc (/ (- p 1) p), 
	 chains (int-array limit (iterate inc 0))
	 sum (long 0) ]
    (if (<= limit p)
      sum
      (if (<= limit i) 
	(let [pp (int p)
	      p (int (update-with-length-up-to-prime p chains))] 
	  (recur (int (* 2 p)) p (/ (- p 1) p) 
		 (if (<= limit p) 
		   chains
		   (do (aset chains p (int (- p 1))) chains))
		 (long (if (= (aget chains pp) length) 
			 ;;(do (println sum pp))
			 (+ sum pp) sum))))
	(recur (+ i p) p calc 
	       (do (aset chains i (int (* (aget chains i) calc))) chains)
	       (long sum))))))
;;problem214> (time (chain-sum 25 40000000))
;; started 20:00
;; expected: 60 min
;; 3248  0 9548417
;; ..
;; 55148 1677326279086 39999857
;; (- 55148 3248)
;; 51900 primes with chainlenght 25
;;"Elapsed time: 3856258.628283 msecs"
;;1677366278943

;; optimizing by adding type hints (and removing printout)

;; problem214> (time (chain-sum 25 40000000))
;; "Elapsed time: 484664.957368 msecs"
;; 1677366278943

;; which brings us down to the initially expected execution time.
;; (based on numbers from problem 72)

(defn init-int-array-iterated [n]
  (let [n (int n) v (int-array n)]
    (loop [i (int 0)]
      (if (< i n)
        (do (aset v i i) (recur (inc i)))
        v))))

;; chains (init-int-array-iterated limit)
;; it is faster to initialize the array this way, but somehow does not work
;; when done inside chain-sum
