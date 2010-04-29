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
	:when (= 200  (+ x1p x2p x5p x10p x20p x50p x100p x200p)) ] 
    (list x1p x2p x5p x10p x20p x50p x100p x200p)))

;; takes incredibly long to execute.

(defn change []
  (for [x1p   (range 0 201 1)   
	x2p   (range 0 (- 201 x1p) 2)
	x5p   (range 0 (- 201 x1p x2p) 5)	
	x10p  (range 0 (- 201 x1p x2p x5p) 10)
	x20p  (range 0 (- 201 x1p x2p x5p x10p) 20)
	x50p  (range 0 (- 201 x1p x2p x5p x10p x20p) 50)
	x100p (range 0 (- 201 x1p x2p x5p x10p x20p x50p) 100) 
	x200p (range 0 (- 201 x1p x2p x5p x10p x20p x50p x100p) 200)
	:when (= 200 (+ x1p x2p x5p x10p x20p x50p x100p x200p)) ] 
    (list x1p x2p x5p x10p x20p x50p x100p x200p)))

;; limiting the ranges does not help at all (especially considering the error in the :when statement)
;; problem031> (time (count (change)))
;; "Elapsed time: 79883.61596 msecs"
;; 73682

(defn change []
  (for [x200p (range 0 201 200) 
	x100p (range 0 (- 201 x200p) 100)
	x50p  (range 0 (- 201 x200p x100p) 50)
	x20p  (range 0 (- 201 x200p x100p x50p) 20)  
	x10p  (range 0 (- 201 x200p x100p x50p x20p) 10)   
	x5p   (range 0 (- 201 x200p x100p x50p x20p x10p) 5)	
	x2p   (range 0 (- 201 x200p x100p x50p x20p x10p x5p) 2)
	x1p   (range 0 (- 201 x200p x100p x50p x20p x10p x5p x2p) 1)   
	:when (= 200  (+ x1p x2p x5p x10p x20p x50p x100p x200p)) ] 
    (list x1p x2p x5p x10p x20p x50p x100p x200p)))

;; flipping the ranges does help!

;; problem031> (time (count (change)))
;; "Elapsed time: 16083.125168 msecs"
;; 73682
