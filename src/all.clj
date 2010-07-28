(ns all
  (:use clojure.test
	problem001 problem002 problem003 problem004 problem005 problem006 problem007 
	problem008 problem009 problem010 problem011 problem012 problem013 problem014
	problem015 problem016 problem017 problem018 problem019 problem020
	problem067))

(deftest test-solve-all
  (is (= (time (problem001)) 233168))              ;;    15.848107 msecs
  (is (= (time (problem002)) 4613732))		   ;;     0.733054 msecs
  (is (= (time (problem003)) 6857))		   ;;     1.016889 msecs
#_(is (= (time (problem004)) 906609))		   ;;  3439.641507 msecs
#_(is (= (time (problem005)) 232792560))	   ;; 65721.427405 msecs
  (is (= (time (problem006)) 25164150))		   ;;     2.360636 msecs
#_(is (= (time (problem007)) 104743))		   ;;  1145.297856 msecs
  (is (= (time (problem008)) 40824))		   ;;  2418.579888 msecs
  (is (= (time (problem009)) 31875000))		   ;;   809.798128 msecs
  (is (= (time (problem010)) 142913828922))	   ;; 53465.519664 msecs
  (is (= (time (problem011)) 70600674))		   ;;    55.231624 msecs
#_(is (= (time (problem012)) 76576500))  	   ;; 39017.057782 msecs
  (is (= (time (problem013)) 5537376230))          ;;     0.165384 msecs
#_(is (= (time (problem014)) 837799))              ;;149366.547031 msecs
  (is (= (time (problem015)) 137846528820))        ;;     0.763784 msecs
  (is (= (time (problem016)) 1366))                ;;     9.828627 msecs
  (is (= (time (problem017)) 21124))               ;;    81.414566 msecs
  (is (= (time (problem018)) 1074))                ;;    10.337071 msecs
  (is (= (time (problem019)) 171))                 ;;    92.623807 msecs
  (is (= (time (problem020)) 648))                 ;;     2.700065 msecs

  (is (= (time (problem067)) 7273))                ;;    24.278234 msecs
)

