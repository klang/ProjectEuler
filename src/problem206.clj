; [1 2 3 4 5 6 7 8 9 0]
;  

(def s (map #(integer 
	 (drop-last 
	  (interleave [1 2 3 4 5 6 7 8 9 0] (cycle [%])))) 
       (range 0 10)))

(map #(exact-integer-sqrt %) s)

;([1010101010 204060800] [1058921220 1450893510] [1105587740 1788975320] [1150362705 582466905] [1193459029 745862099] [1235052450 1406583450] [1275290028 1151446176] [1314296297 1369875761] [1352177820 1787936580] [1389026623 295205861])

;; user> (count (selections [1 2 3 4 5] 5))
;; 3125
;; user> (count (selections [1 2 3 4 ] 4))
;; 256
;; user> (count (selections [1 2 3  ] 3))
;; 27
;; user> (expt 5 5)
;; 3125
;; user> (expt 10 10)
;; 10000000000

;; (exact-integer-sqrt n) ==> [1_2_3_4_5_6_7_8_9_0 0]
;; number of digits in sqrt: 19
;; first digit:               1
;; last digit:                0