(ns euler.core
  (:use [problem001 :only (problem001)] [problem002 :only (problem002)]	[problem003 :only (problem003)]
	[problem004 :only (problem004)]	[problem005 :only (problem005)] [problem006 :only (problem006)]
	[problem007 :only (problem007)] [problem008 :only (problem008)]	[problem009 :only (problem009)]
	[problem010 :only (problem010)] [problem011 :only (problem011)] [problem012 :only (problem012)]
	[problem013 :only (problem013)] [problem014 :only (problem014)]	[problem015 :only (problem015)]
	[problem016 :only (problem016)]	[problem017 :only (problem017)] [problem018 :only (problem018)]
	[problem019 :only (problem019)] [problem020 :only (problem020)] #_[problem021 :only (problem021)]
	[problem022 :only (problem022)] [problem023 :only (problem023)] [problem024 :only (problem024)]
	[problem025 :only (problem025)] [problem026 :only (problem026)] [problem027 :only (problem027)]
	[problem028 :only (problem028)] [problem029 :only (problem029)] #_[problem030 :only (problem030)]
	[problem031 :only (problem031)] #_[problem032 :only (problem032)] [problem033 :only (problem033)]
	[problem034 :only (problem034)] [problem035 :only (problem035)] [problem036 :only (problem036)]
	[problem037 :only (problem037)] [problem038 :only (problem038)] [problem039 :only (problem039)]
	[problem040 :only (problem040)] [problem041 :only (problem041)] [problem042 :only (problem042)]
	[problem043 :only (problem043)] [problem044 :only (problem044)] [problem045 :only (problem045)]
	[problem046 :only (problem046)] [problem047 :only (problem047)] [problem048 :only (problem048)]

	[problem049 :only (problem049)] [problem050 :only (problem050)]
	[problem054 :only (problem054)]
	[problem061 :only (problem061)]
	[problem067 :only (problem067)] [problem068 :only (problem068)]
	[problem081 :only (problem081)] [problem082 :only (problem082)] [problem083 :only (problem083)]
	[problem093 :only (problem093)]
	[problem119 :only (problem119)]
	[problem087 :only (problem087)]
	[problem158 :only (problem158)]
	[problem108 :only (problem108)] [problem110 :only (problem110)]
	[problem231 :only (problem231)]
	))

(comment
  (-> (pcalls problem001 problem002 problem003 problem004 problem005
	      problem006 problem007 problem008 problem009 problem010
	      problem011 problem012 problem013 problem014 problem015
	      problem016 problem017 problem018 #_problem019 #_problem020) doall time)
;;  "Elapsed time: 206665.901925 msecs"
;;  (233168 4613732 6857 906609 232792560 25164150 104743 40824 31875000 142913828922 70600674 76576500 5537376230 837799 137846528820 1366 21124 1074))


(comment
  (-> (pvalues (problem001) (problem002) (problem003)
	       (problem004) (problem005) (problem006)
	       (problem007) (problem008) (problem009)) doall time))

(defmacro time-hash
  "Evaluates expr and retuns {:result expr-result :time evaluation-time}."
  [expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr]
     {:time (/ (double (- (. System (nanoTime)) start#)) 1000000.0) :result ret#}))

(defmacro time-id
  [id expr]
  `(hash-map (keyword (str ~id)) (time-hash ~expr)))


(def euler
     (into {}
	   (-> (pvalues
		(time-id "001" (problem001)) (time-id "002" (problem002)) (time-id "003" (problem003))
		(time-id "004" (problem004)) (time-id "005" (problem005)) (time-id "006" (problem006))
		(time-id "007" (problem007)) (time-id "008" (problem008)) (time-id "009" (problem009)))
	       doall time)))