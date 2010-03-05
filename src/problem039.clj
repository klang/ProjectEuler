(use 'clojure.contrib.math)

(defn solutions [p]
  ;; partition p in 3 groups where a + b + c = p
  ;; and a² + b² = c² 
  ;; <=> (exact-integer-sqt (+ (* a a) (* b b))) = (c 0)
  )

(defn solutions [p]
     (for [a (range 1 p) 
	   b (range a p) 
	   :when (and (zero? (second (exact-integer-sqrt (+ (* a a) (* b b)))))
		      (= (+ a b (first (exact-integer-sqrt (+ (* a a) (* b b))))) p))]
        [a b (first (exact-integer-sqrt (+ (* a a) (* b b))))]))

;(filter #(not (zero? (second %))) (map (fn [a] [a (count (tri a))]) (range 1 20)))

(defn p39 [limit]
  (loop [mxt 0 mxi 0 i 1]
    (if (< limit i)
      (list mxi mxt)
      (let [tr (count (solutions i))]
	(if (<= mxt tr)
	  ;(do (println (list i mxt mxi)))
	  (recur tr i (inc i))
	  (recur mxt mxi (inc i)))))))

;; user> (time (p39 1000))
;; (1 0 0)
;; (2 0 1)
;; (3 0 2)
;; (4 0 3)
;; (5 0 4)
;; (6 0 5)
;; (7 0 6)
;; (8 0 7)
;; (9 0 8)
;; (10 0 9)
;; (11 0 10)
;; (12 0 11)
;; (24 1 12)
;; (30 1 24)
;; (36 1 30)
;; (40 1 36)
;; (48 1 40)
;; (56 1 48)
;; (60 1 56)
;; (84 2 60)
;; (90 2 84)
;; (120 2 90)
;; (168 3 120)
;; (180 3 168)
;; (240 3 180)
;; (360 4 240)
;; (420 4 360)
;; (660 5 420)
;; (720 5 660)
;; (840 6 720)
;; "Elapsed time: 1059801.763877 msecs"
;; (840 8)

(defn p039 [p]
  ;; a + b + c = p
  ;; and a² + b² = c² 
  ;; b = (p^2 - 2(p*a)) / 2(p-a) 
  ;; Thus if (p^2 - 2(p*a)) % 2(p-a) == 0 we have a solution 
  ;; From symmetry, we need only check up to p/4
  ;; smax = 0,0 # num solutions, perimeter
  ;; for p in range(1,1000):
  ;;     t = 0
  ;;     for a in range(2,p/4+1):
  ;;         if (p*p - 2*p*a) % (2*p-2*a) == 0:
  ;;             t += 1
  ;;     if t > smax[0]:
  ;;         smax = t, p
  ;; print smax
  )

;; (for [a (range 2, (+ (/ p 4) 1)) :when (zero? (mod (- (* p p) (* 2 p a)) (- (* 2 p) (* 2 a))))] 1) 

(defn p39 [limit]
  (loop [mxt 0 mxi 0 p 1]
    (if (< limit p)
      mxi
      (let [t (reduce + 
		        (for [a (range 2, (+ (/ p 4) 1))] 
			  (if (zero? (mod (- (* p p) 
					     (* 2 p a)) 
					  (- (* 2 p) (* 2 a)))) 
			    1 0)))]
	(if (< mxt t)
	  (recur t p (inc p))
	  (recur mxt mxi (inc p)))))))

;; user> (time (p39 1000))
;; "Elapsed time: 555.710465 msecs"
;; 840
;; much much better
