(ns problem200
  (meta {:description
	 "We shall define a sqube to be a number of the form, p2q3, where p and q are distinct primes.
For example, 200 = 5223 or 120072949 = 232613.

The first five squbes are 72, 108, 200, 392, and 500.

Interestingly, 200 is also the first number for which you cannot change any single digit to make a prime; we shall call such numbers, prime-proof. The next prime-proof sqube which contains the contiguous sub-string '200' is 1992008.

Find the 200th prime-proof sqube containing the contiguous sub-string '200'."
	 :hint
	 "I'm not sure this is a great SO question, but this should get you started at least.

Algorithmically, the brute force and ignorance approach to this should be pretty straightforward:

Start with set of primes P
Generate 'squbes' by looking at all ordered pairs (p1,p2) with p1,p2 in P
Order this list, call the set S (think: how can you do this while generating them)
Test each s in S in turn, looking for substring 200
If s contains '200' test each one-digit modification of s to see if it's prime
If none of them are prime, you've found a prime-proof sqube. Put it in a list, and when you've found the 200th one you're done.
Now the more interesting thing here is to see if you can do something a bit less brute force and ignorance. First off, is the above even practical (assuming a fast primality test)? Easy enough to estimate how fast the squbes grow, but not so much with the prime-proof ones, or the ones containing 200. Can you see any shortcuts?"})
  (:use [clojure.contrib.combinatorics :only (combinations)]
	[clojure.contrib.lazy-seqs :only (primes)])
  )

(defn sqube [[ p q]]
  (* (* p p) (* q q q)))

(map (fn [[p q]] (* (* p p) (* q q q))) 
     (combinations (take 5 primes) 2))

(defn sqube [[p q]]
  (let [a (* p p)
	b (* q q)]
    (list (* a p b) (* a q b))))

(map #(sqube %)
     (combinations (take 5 primes) 2))

(defn squbes [limit] 
  (sort (reduce concat 
		(map (fn [[p q]] (let [a (* p p) b (* q q)]
				   (list (* a p b) (* a q b)))) 
		     (combinations (take limit primes) 2)))))

(defn squbes-count [limit] (* 2 limit (- limit 1)))

(def prime-proof nil)

(defn substring-200 [number]
  (<= 0 (.indexOf (.toString number) "200")))

;; (filter #(substring-200 %) ( squbes 10))

;; user> (filter #(substring-200 %) (squbes 50))
;; (200 96632003 120072949 2005909453 2005964183 4823420099 20045656993 24152002213 34320079397 42239200781 200495610673 200733641363 200942019053 320064006697 372009061531)

;;user> (filter #(= 1992008 %) (squbes 50))
;;()
;; 1992008 is missing from this list .. why?