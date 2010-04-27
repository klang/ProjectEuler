(ns problem031
  (meta 
   {:description "In England the currency is made up of pound, £, and pence, p, and there are eight coins in general circulation:

1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
It is possible to make £2 in the following way:

1x£1 + 1x50p + 2x20p + 1x5p + 1x2p + 3x1p
How many different ways can £2 be made using any number of coins?
"
    :help "http://www.perlmonks.org/index.pl?node_id=406984"}))

(defn change []
  (for [x1p   (range 0 201 1)   x2p   (range 0 201 2)
	x5p   (range 0 201 5)	x10p  (range 0 201 10)
	x20p  (range 0 201 20)	x50p  (range 0 201 50)
	x100p (range 0 201 100) x200p (range 0 201 200)
	:when (= 200  (+ x1p x2p 
		       x5p x10p x20p x50p x100p x200p)) ] 
    (list x1p x2p x5p x10p x20p x50p x100p x200p)))

;; takes incredibly long to execute.

(defn change []
  (for [x1p   (range 0 201 1)   
	x2p   (range 0 (+ (- 200 x1p) 1) 2)
	x5p   (range 0 (+ (- 200 x1p x2p) 1) 5)	
	x10p  (range 0 (+ (- 200 x1p x2p x5p) 1) 10)
	x20p  (range 0 (+ (- 200 x1p x2p x5p x10p) 1) 20)
	x50p  (range 0 (+ (- 200 x1p x2p x5p x10p x20p) 1) 50)
	x100p (range 0 (+ (- 200 x1p x2p x5p x10p x20p x50p) 1) 100) 
	x200p (range 0 (+ (- 200 x1p x2p x5p x10p x20p x50p x100p) 1) 200)
	:when (= 200  (+ x1p x2p x1p x5p x10p x20p x50p x100p x200p)) ] 
    (list x1p x2p x5p x10p x20p x50p x100p x200p)))

;; limiting the ranges does not help at all

(defn change []
  (for [x1p   (range 0 201 1)   
	x2p   (range 0 (+ (- 200 x1p) 1) 2)]
    (for [x5p   (range 0 (+ (- 200 x1p x2p) 1) 5)	
	  x10p  (range 0 (+ (- 200 x1p x2p x5p) 1) 10)
	  x20p  (range 0 (+ (- 200 x1p x2p x5p x10p) 1) 20)
	  x50p  (range 0 (+ (- 200 x1p x2p x5p x10p x20p) 1) 50)
	  x100p (range 0 (+ (- 200 x1p x2p x5p x10p x20p x50p) 1) 100) 
	  x200p (range 0 (+ (- 200 x1p x2p x5p x10p x20p x50p x100p) 1) 200)
	  :when (= 200  (+ x1p x2p x1p x5p x10p x20p x50p x100p x200p)) ] 
      (list x1p x2p x5p x10p x20p x50p x100p x200p))))

;; problem031> (time (count (change)))
;; "Elapsed time: 61.178753 msecs"
;; 10201

(defn change []
  (for [x1p   (range 0 201 1)     x2p   (range 0 201 2)]
    (for [x5p   (range 0 201 5)	  x10p  (range 0 201 10)
	  x20p  (range 0 201 20)  x50p  (range 0 201 50)
	  x100p (range 0 201 100) x200p (range 0 201 200)
	  :when (= 200  (+ x1p x2p x5p x10p x20p x50p x100p x200p)) ] 
      (list x1p x2p x5p x10p x20p x50p x100p x200p))))
;; problem031> (time (count (change)))
;; "Elapsed time: 57.548397 msecs"
;; 20301
;; (* 201 101)
;; the ranges are wrong

;; Instead of checking which result is the correct one, we try to figure out another way to do this.
;; aim for --> 39224 (problem031.pl, home to mama)

