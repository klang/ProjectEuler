(ns all
  (meta {:description ""})
  (:use [clojure.test :only (deftest is run-tests)]
        [euler.core :only (solved broken time-hash time-id problem)]
	;; read in all the problems, but in each case, only the special function that returns the result
	;; to avoid support function name clashing.
	[problem001 :only (problem001)] [problem002 :only (problem002)]	[problem003 :only (problem003)]
	[problem004 :only (problem004)]	[problem005 :only (problem005)] [problem006 :only (problem006)]
	[problem007 :only (problem007)] [problem008 :only (problem008)]	[problem009 :only (problem009)]
	[problem010 :only (problem010)] [problem011 :only (problem011)] [problem012 :only (problem012)]
	[problem013 :only (problem013)] [problem014 :only (problem014)]	[problem015 :only (problem015)]
	[problem016 :only (problem016)]	[problem017 :only (problem017)] [problem018 :only (problem018)]
	[problem019 :only (problem019)] [problem020 :only (problem020)] [problem021 :only (problem021)]
	[problem022 :only (problem022)]	[problem023 :only (problem023)]	[problem024 :only (problem024)]
	[problem025 :only (problem025)] [problem026 :only (problem026)] [problem027 :only (problem027)]
	[problem028 :only (problem028)]	[problem029 :only (problem029)]	[problem030 :only (problem030)]
	[problem031 :only (problem031)] [problem032 :only (problem032)] [problem033 :only (problem033)]
	[problem034 :only (problem034)] [problem035 :only (problem035)]	[problem036 :only (problem036)]
	[problem037 :only (problem037)] [problem038 :only (problem038)] [problem039 :only (problem039)]
	[problem040 :only (problem040)]	[problem041 :only (problem041)] [problem042 :only (problem042)]
	[problem043 :only (problem043)] [problem044 :only (problem044)] [problem045 :only (problem045)]
	[problem046 :only (problem046)] [problem047 :only (problem047)] [problem048 :only (problem048)]
	[problem049 :only (problem049)] [problem050 :only (problem050)] #_[problem051 :only (problem051)]
	[problem052 :only (problem052)] [problem053 :only (problem053)] [problem054 :only (problem054)]
	[problem055 :only (problem055)] [problem056 :only (problem056)] [problem057 :only (problem057)]
	[problem058 :only (problem058)] [problem059 :only (problem059)] [problem060 :only (problem060)]
	[problem061 :only (problem061)] [problem062 :only (problem062)] [problem063 :only (problem063)]
	[problem064 :only (problem064)] [problem065 :only (problem065)] #_[problem066 :only (problem066)]
	[problem067 :only (problem067)] [problem068 :only (problem068)] [problem069 :only (problem069)]
        [problem070 :only (problem070)] [problem071 :only (problem071)] [problem072 :only (problem072)] 
        [problem073 :only (problem073)] [problem074 :only (problem074)] [problem075 :only (problem075)] 
        [problem076 :only (problem076)] #_[problem077 :only (problem077)] [problem078 :only (problem078)] 
        [problem079 :only (problem079)] [problem080 :only (problem080)]	[problem081 :only (problem081)]
        [problem082 :only (problem082)] [problem083 :only (problem083)]

	[problem085 :only (problem085)]	[problem087 :only (problem087)] [problem089 :only (problem089)]
	[problem092 :only (problem092)] [problem093 :only (problem093)]

        [problem096 :only (problem096)] [problem097 :only (problem097)] [problem098 :only (problem098)]
        [problem099 :only (problem099)] [problem102 :only (problem102)] [problem104 :only (problem104)]
	[problem108 :only (problem108)] [problem110 :only (problem110)]
        [problem112 :only (problem112)] [problem120 :only (problem120)] [problem122 :only (problem122)]
        [problem123 :only (problem123)] [problem124 :only (problem124)] [problem125 :only (problem125)] 
        [problem145 :only (problem145)] [problem160 :only (problem160)] [problem179 :only (problem179)] 
        [problem187 :only (problem187)] [problem188 :only (problem188)] [problem197 :only (problem197)] 
        [problem203 :only (problem203)] [problem204 :only (problem204)] [problem206 :only (problem206)] 
        [problem214 :only (problem214)]	[problem119 :only (problem119)] [problem158 :only (problem158)]
	[problem231 :only (problem231)]	[problem265 :only (problem265)]	[problem297 :only (problem297)]
	[problem315 :only (problem315)]	[problem317 :only (problem317)] [problem321 :only (problem321)]))

(defmacro time
  "Evaluates expr and prints the time it took.  Returns the value of
 expr."
  {:added "1.0"}
  [expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr]
     (prn (str (quote ~expr) " Elapsed time: " (/ (double (- (. System (nanoTime)) start#)) 1000000.0) " msecs"))
     ret#))

;; time measurements
;; IBM X40 = 1.5 GHz 1 core cpu with jvm-opts -Xmx512M  
;; MacBook = 2.7 Ghz 8 core cpu
(deftest test-solve-all                            ;;    IBM X40                MacBook
  (is (= (time (problem001)) 233168))	           ;;    15.848107 msecs
  (is (= (time (problem002)) 4613732))             ;;     0.733054 msecs
  (is (= (time (problem003)) 6857))                ;;     1.016889 msecs
  (is (= (time (problem004)) 906609))              ;;  3439.641507 msecs
  (is (= (time (problem005)) 232792560))           ;;     0.900674 msecs
  (is (= (time (problem006)) 25164150))            ;;     2.360636 msecs
  (is (= (time (problem007)) 104743))              ;;  1145.297856 msecs
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
  (is (= (time (problem021)) 31626))               ;;
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
;;(is (= (time (problem051)) 0))
  (is (= (time (problem052)) 142857))
  (is (= (time (problem053)) 4075))
  (is (= (time (problem054)) 376))                 ;;  2325.333083 msecs
  (is (= (time (problem055)) 249))                 ;;   869.360469 msecs     168.323 msecs
  (is (= (time (problem056)) 972))                 ;;                        466.396 msecs
#_(is (= (time (problem057)) 153))                 ;;170892.443036 msecs   33334.296 msecs
#_(is (= (time (problem058)) 26241))
  (is (= (time (problem059)) 107359))
#_(is (= (time (problem060)) 26033))               ;;796469.559427 msecs  197791.383 msecs
  (is (= (time (problem061)) 28684))
  (is (= (time (problem062)) 127035954683))
  (is (= (time (problem063)) 49))
  (is (= (time (problem064)) 1322))
  (is (= (time (problem065)) 272))
;;(is (= (time (problem066)) 0))
  (is (= (time (problem067)) 7273))                ;;    24.278234 msecs
#_(is (= (time (problem068)) 6531031914842725))    ;; 81512.760641 msecs
  (is (= (time (problem069)) 510510))
#_(is (= (time (problem070)) 8319823))             ;;5498767.682737 msecs
  (is (= (time (problem071)) 428570))              ;;  1867.093735 msecs     665.069 msecs
#_(is (= (time (problem072)) 303963152391))        ;;                      19872.972 msecs
#_(is (= (time (problem073)) 7295373))             ;;                      85724.065 msecs
#_(is (= (time (problem074)) 402))                 ;;                     191819.953 msecs
  (is (= (time (problem075)) 161667))              ;; 
  (is (= (time (problem076)) 190569292))           ;;    82.583742 msecs       7.068 msecs
;;(is (= (time (problem077)) 0))                   ;;
;;(is (= (time (problem078)) 55374))               ;; index-lookup (cheating)
  (is (= (time (problem079)) 73162890))            ;; pen and paper
  (is (= (time (problem080)) 40886))               ;;                         12.083 msecs
  (is (= (time (problem081)) 427337))              ;;   130.25124 msecs
  (is (= (time (problem082)) 260324))              ;;   691.370197 msecs      
  (is (= (time (problem083)) 425185))              ;;   701.88803 msecs 
  (is (= (time (problem087)) 1097343))             ;;     0.103365 msecs
  (is (= (time (problem085)) 2772))                ;;
  (is (= (time (problem089)) 5850))                ;;
#_(is (= (time (problem092)) 8581146))             ;;176651.667125 msecs   34527.446 msecs
#_(is (= (time (problem093)) 1258))                ;; 14724.167179 msecs
  (is (= (time (problem096)) 24702))
  (is (= (time (problem097)) 8739992577))
  (is (= (time (problem098)) 18769))
  (is (= (time (problem099)) :unknown))
  (is (= (time (problem102)) 228))
#_(is (= (time (problem104)) 329468))              ;; 197231.24822  msec    28175.777 msecs
  (is (= (time (problem112)) 1587000))             ;;  15708.773854 msecs    6400.604 msecs
  (is (= (time (problem120)) 333082500))
  (is (= (time (problem122)) 1582))
  (is (= (time (problem123)) 21035))
  (is (= (time (problem124)) 21417))               ;;  25978.545876 msecs    4297.119 msecs
  (is (= (time (problem125)) 2906969179))          ;;   4049.981541 msecs    1065.568 msecs
  (is (= (time (problem145)) 608720))
  (is (= (time (problem160)) 16576))
#_(is (= (time (problem179)) 986262))              ;;  38507.965323 msecs   49581.552 msecs
  (is (= (time (problem187)) 17427258))            ;;   6620.393309 msecs    1114.102 msecs
  (is (= (time (problem188)) 95962097))            ;;    208.373776 msecs      24.391 msecs
  (is (= (time (problem197)) 1.710637717))         ;;
  (is (= (time (problem203)) 34029210557338)) 
#_(is (= (time (problem204)) 2944730))             ;; 305696.680426 msecs    40401.327 msecs
  (is (= (time (problem206)) 1929374254627488900)) ;; 1819.530397 msecs 738.79 msecs
#_(is (= (time (problem214)) 0))  
  (is (= (time (problem108)) 180180))              ;;  4707.300194 msecs
#_(is (= (time (problem110)) 9350130049860600))    ;; 13919.281935 msecs
  (is (= (time (problem119)) 248155780267521))     ;;    25.218576 msecs
  (is (= (time (problem158)) 409511334375))        ;;    17.227335 msecs
  (is (= (time (problem231)) 7526965179680))       ;;  5021.874612 msecs + 6 seconds to generate primes up to 20 million
#_(is (= (time (problem265)) 209110240768))        ;; 126994.129502 msecs  32780.248 msecs
  (is (= (time (problem297)) 7894453))             ;;                          1.431 msecs
#_(is (= (time (problem315)) 30706772))            ;;  35701.360078 msecs  11543.467 msecs
  (is (= (time (problem317)) 1856532.8455))        ;;
  (is (= (time (problem321)) 2470433131948040))    ;;                          0.085 msecs
  

  ;; all> (time (run-tests))
  ;; "Elapsed time: 498687.448741 msecs"
  ;; {:type :summary, :test 1, :pass 36, :fail 0, :error 0}
)
(comment
  (clojure.pprint/pprint 
   (map #(list 'is (list '= (list 'time (list (symbol (str "problem" (format "%03d"%)))))'0)) 
        [96 97 98 99 102 104  112 120 122 123 124 125 145 160 179 187 188 197 203 204 206 214]))
  (clojure.pprint/pprint 
   (map #(vector % ':only (list %)) (map #(symbol (str "problem" (format "%03d"%))) 
                                         [96 97 98 99 102 104  112 120 122 123 124 125 145 160 179 187 188 197 203 204 206 214])))
  )

(defn peuler []
  (into {}
        (->>
         solved
         (pmap #(time-id % (problem %)))
         doall
         time)))

(defn seuler []
  (into {}
        (->>
         solved
         (map #(time-id % (problem %)))
         doall
         time)))


