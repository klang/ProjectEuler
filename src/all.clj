(ns all
  (meta {:description ""})
  (:use clojure.test
	;; read in all the problems, but in each case, only the special function that returns the result
	;; to avoid support function name clashing.
	[problem001 :only (problem001)] [problem002 :only (problem002)]	[problem003 :only (problem003)]
	[problem004 :only (problem004)]	[problem005 :only (problem005)] [problem006 :only (problem006)]
	[problem007 :only (problem007)] [problem008 :only (problem008)]	[problem009 :only (problem009)]
	[problem010 :only (problem010)] [problem011 :only (problem011)] [problem012 :only (problem012)]
	[problem013 :only (problem013)] [problem014 :only (problem014)]	[problem015 :only (problem015)]
	[problem016 :only (problem016)]	[problem017 :only (problem017)] [problem018 :only (problem018)]
	[problem019 :only (problem019)] [problem020 :only (problem020)] #_[problem021 :only (problem021)]
	[problem022 :only (problem022)]	[problem023 :only (problem023)]	[problem024 :only (problem024)]
	[problem025 :only (problem025)] [problem026 :only (problem026)] [problem027 :only (problem027)]
	[problem028 :only (problem028)]	[problem029 :only (problem029)]	#_[problem030 :only (problem030)]
	[problem031 :only (problem031)] #_[problem032 :only (problem032)] [problem033 :only (problem033)]
	[problem034 :only (problem034)] [problem035 :only (problem035)]	[problem036 :only (problem036)]
	[problem037 :only (problem037)] [problem038 :only (problem038)] [problem039 :only (problem039)]
	[problem040 :only (problem040)]	[problem041 :only (problem041)] [problem042 :only (problem042)]
	[problem043 :only (problem043)] [problem044 :only (problem044)] [problem045 :only (problem045)]
	[problem046 :only (problem046)] [problem047 :only (problem047)] [problem048 :only (problem048)]
	[problem049 :only (problem049)] [problem050 :only (problem050)]
	[problem061 :only (problem061)]
	[problem067 :only (problem067)]
	[problem087 :only (problem087)] [problem087 :only (problem158)]))

(deftest test-solve-all
  (is (= (time (problem001)) 233168))	           ;;    15.848107 msecs
  (is (= (time (problem002)) 4613732))             ;;     0.733054 msecs
  (is (= (time (problem003)) 6857))                ;;     1.016889 msecs
#_(is (= (time (problem004)) 906609))              ;;  3439.641507 msecs
  (is (= (time (problem005)) 232792560))           ;;     0.900674 msecs
  (is (= (time (problem006)) 25164150))            ;;     2.360636 msecs
#_(is (= (time (problem007)) 104743))              ;;  1145.297856 msecs
  (is (= (time (problem008)) 40824))               ;;  2418.579888 msecs
  (is (= (time (problem009)) 31875000))            ;;   809.798128 msecs
#_(is (= (time (problem010)) 142913828922))        ;; 53465.519664 msecs
  (is (= (time (problem011)) 70600674))            ;;    55.231624 msecs
#_(is (= (time (problem012)) 76576500))            ;; 39017.057782 msecs
  (is (= (time (problem013)) 5537376230))          ;;     0.165384 msecs
#_(is (= (time (problem014)) 837799))              ;;149366.547031 msecs
  (is (= (time (problem015)) 137846528820))        ;;     0.763784 msecs
  (is (= (time (problem016)) 1366))                ;;     9.828627 msecs
  (is (= (time (problem017)) 21124))               ;;    81.414566 msecs
  (is (= (time (problem018)) 1074))                ;;    10.337071 msecs
  (is (= (time (problem019)) 171))                 ;;    92.623807 msecs
  (is (= (time (problem020)) 648))                 ;;     2.700065 msecs
;;(is (= (time (problem021)) 0))               ;;
  (is (= (time (problem022)) 871198282))           ;;   103.376583 msecs
#_(is (= (time (problem023)) 4179871))             ;; 47368.821638 msecs
  (is (= (time (problem024)) 2783915460))          ;;  5103.595677 msecs
  (is (= (time (problem025)) 4782))                ;;   730.76045 msecs
  (is (= (time (problem026)) 983))                 ;;   483.818924 msecs
  (is (= (time (problem027)) -59231))              ;; 14060.032308 msecs
  (is (= (time (problem028)) 669171001))           ;;    41.984958 msecs
  (is (= (time (problem029)) 9183))                ;;   186.110873 msecs
;;(is (= (time (problem030)) 0))               ;;
  (is (= (time (problem031)) 73682))               ;; 14999.629352 msecs
;;(is (= (time (problem032)) 0))               ;;
  (is (= (time (problem033)) 1/100))               ;;     8.186797 msecs
#_(is (= (time (problem034)) 40730))               ;; 43909.004401 msecs
#_(is (= (time (problem035)) 55))                  ;; 14526.914683 msecs
  (is (= (time (problem036)) 872187))              ;;  7750.714134 msecs
#_(is (= (time (problem037)) 748317))              ;; 26726.564428 msecs
  (is (= (time (problem038)) 932718654))           ;;  1206.163197 msecs
  (is (= (time (problem039)) 840))                 ;;   576.700288 msecs
  (is (= (time (problem040)) 210))                 ;;   815.002705 msecs
  (is (= (time (problem041)) 7652413))             ;;     7.161807 msecs
  (is (= (time (problem042)) 162))                 ;;    76.690775 msecs
#_(is (= (time (problem043)) 16695334890))         ;;107738.584877 msecs
#_(is (= (time (problem044)) 5482660))             ;; 31000.114285 msecs
  (is (= (time (problem045)) 1533776805))          ;;  5223.163647 msecs
  (is (= (time (problem046)) 5777))                ;;  3633.234869 msecs
  (is (= (time (problem047)) 134043))              ;; 23164.887237 msecs
  (is (= (time (problem048)) 9110846700))          ;;   397.577470 msecs
  (is (= (time (problem049)) 296962999629))        ;;   102.765612 msecs
#_(is (= (time (problem050)) 997651))              ;; 95012.780427 msecs
  (is (= (time (problem061)) 20604))
  (is (= (time (problem087)) 743))                 ;;     0.103365 msecs
  (is (= (time (problem067)) 7273))                ;;    24.278234 msecs
  (is (= (time (problem158)) 409511334375))        ;;    17.227335 msecs
)

;; all> (time (run-tests))
;; "Elapsed time: 498687.448741 msecs"
;; {:type :summary, :test 1, :pass 36, :fail 0, :error 0}