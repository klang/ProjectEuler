(load "tools")

(defn number-shift-right [number]
  (let [ds (digits number)
	d (first ds)
	s (rest ds)]
;;  (BigInteger. (str (apply str s) d))
    (Integer. (str (apply str s) d))))

(defn rotations [number]
  (let [ds (digits number)]
    (map #(str (apply str (drop % ds)) 
	       (apply str (take % ds))) 
	 (range 0 (count ds)))))

(defn circular [number]
  (let [rot (map #(Integer. %) (rotations number))
	limit (reduce max rot)]
    (reduce #(and %1 %2) (map #(contains? (set (take limit primes)) %) rot) ))
  )

;; very fast to pre calculate and will reduce the number of possible circular primes significantly
(defn circular-odd? [number]
  (let [rot (map #(Integer. %) (rotations number))]
    (reduce #(and %1 %2) (map #(odd? %) rot) )))

;; not especially efficient because of the inefficient way to determin if a number is a prime.
(defn circular-prime? [number]
  (let [rot (map #(Integer. %) (rotations number))]
    (reduce #(and %1 %2) (map #(prime? %) rot) )))

(defn circular [number primeset]
  (let [rot (map #(Integer. %) (rotations number))]
    (reduce #(and %1 %2) (map #(contains? primeset %) rot))))

(defn circular-primes [number]
  (let [circular-odd-set 
	(filter #(< 0 %) (map #(if (circular-odd? %) % 0) (take-while #(<  % number) primes)))]
    (+ 1 (reduce + (map #(if (circular-prime? %) 1 0) circular-odd-set)))))

(def circular-odd-set 
     (filter #(< 0 %) (map #(if (circular-odd? %) % 0) (take-while #(<  % 1000000) primes))))
;; user> (time (+ 1 (reduce + (map #(if (circular-prime? %) 1 0) circular-odd-set))))
;; "Elapsed time: 322089.503076 msecs"
;; 55
;; user> (time (circular-primes 1000000))
;; "Elapsed time: 336150.768137 msecs"
;; 55

;; -- trying to make things a bit more efficient (goal: less than a minute)

;; if we always rotate prime numbers we do not need them in the rotation set
;; rotations of number, but not number itself. 
(defn rotations [number]
  (let [ds (digits number)]
    (map #(str (apply str (drop % ds)) 
	       (apply str (take % ds))) 
	 (range 1 (count ds))))) 

;; let us save the Integer conversion on numbers we do not need
(defn circular-odd? [number]
  (let [rot (map #(Integer. %) (rotations number))
	rot (rotations number)
	]
    (reduce #(and %1 %2) (map #(odd? %) rot) )))
(for [n ])
(defn circular-prime? [number]
  (let [rot (map #(Integer. %) (rotations number))]
    (reduce #(and %1 %2) (map #(prime? %) rot) )))

(defn maybe-circular-prime? [number]
  (let [rot (map #(Integer. %) (rotations number))]
    (reduce #(and %1 %2) (map #(odd? %) rot) )))

(filter #(< 0 %) (map #(if (maybe-circular-prime? %) % 0) (take-while #(<  % 1000000) primes)))
