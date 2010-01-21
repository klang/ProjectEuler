; The following iterative sequence is defined for the set of positive integers:

; n -> n/2,     (n is even)
; n -> 3n + 1,  (n is odd)

;Using the rule above and starting with 13, we generate the following sequence:

; 13 -> 40 -> 20 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1
;It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.

;Which starting number, under one million, produces the longest chain?

(defn p014 [n]
  (loop [n n
	 s []]
    (if (= 1 n)
      (conj s 1)
      (if (even? n) 
	(recur (quot n 2) (conj s n))
	(recur (+ (* 3 n) 1) (conj s n))
	))))

;; the same, but more compact
(defn collatz [n]
  (loop [n n, s []]
    (if (= 1 n)
      (conj s n)
      (recur 
       (if (even? n) 
	 (quot n 2) 
	 (+ (* 3 n) 1)) 
       (conj s n)))))

(defn num-terms-collatz [n]
  (let [c (collatz n)
	l (count c)]
    {:terms l :start n}))

(defn max-collatz [limit]
  (loop [n 1
	 m (num-terms-collatz n)
	 c (num-terms-collatz n)]
    (if (= n limit)
      m
      (if (< (:terms m) (:terms c))
	(recur (inc n) c (num-terms-collatz (inc n)))
	(recur (inc n) m (num-terms-collatz (inc n)))
	))
    ))
;; user> (time (max-collatz 1000000))
;; "Elapsed time: 139867.110222 msecs"
;; {:terms 525, :start 837799}

(defn max-collatz [limit]
  (loop [n 1
	 m (num-terms-collatz n)
	 c (num-terms-collatz n)]
    (if (= n limit)
      m
      (recur (inc n) 
	     (if (< (:terms m) (:terms c)) c m)
	     (num-terms-collatz (inc n))))))

;; user> (time (max-collatz 1000000))
;; "Elapsed time: 144540.263286 msecs"
;; {:terms 525, :start 837799}

(defn max-collatz [limit]
  (loop [n 1
	 m {:start 1 :terms 1}
	 c {:start 1 :terms 1}]
    (if (= n limit)
      m
      (recur (inc n) 
	     (if (< (:terms m) (:terms c)) c m)
	     {:start (inc n) :terms (count (collatz (inc n)))}))))
;; user> (time (max-collatz 1000000))
;; "Elapsed time: 149366.547031 msecs"
;; {:start 837799, :terms 525}



   