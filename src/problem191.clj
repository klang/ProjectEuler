(def trinary {:O 0 :L 1 :A 2})
(def trinary {0 \O 1 \L 2 \A})

;; L
;; AA

;; 4 days
;; (expt 3 4)
;; 81

;; 43 prize strings
(def  prize 
      "OOOO" "OOOA" "OOOL" "OOAO" "OOAA" "OOAL" "OOLO" "OOLA" "OAOO" "OAOA"
      "OAOL" "OAAO" "OAAL" "OALO" "OALA" "OLOO" "OLOA" "OLAO" "OLAA" "AOOO"
      "AOOA" "AOOL" "AOAO" "AOAA" "AOAL" "AOLO" "AOLA" "AAOO" "AAOA" "AAOL"
      "AALO" "AALA" "ALOO" "ALOA" "ALAO" "ALAA" "LOOO" "LOOA" "LOAO" "LOAA"
      "LAOO" "LAOA" "LAAO")

;; 0000 0001 0002 0010 0011 0012 0020 0021 0100 0101
;; 0102 0110 0112 0120 0121 0200 0201 0210 0211 1000
;; 1001 1002 1010 1011 1012 1020 1021 1100 1101 1102
;; 1120 1121 1200 1201 1210 1211 2000 2001 2010 2011
;; 2100 2101 2110

;; 30 days 
;;user> (expt 3 30)
;;205891132094649