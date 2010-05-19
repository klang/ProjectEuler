(ns problem038
  (meta {:description "Take the number 192 and multiply it by each of 1, 2, and 3:

      192 x 1 = 192
      192 x 2 = 384
      192 x 3 = 576
      By concatenating each product we get the 1 to 9 pandigital, 192384576. We will call 192384576 the concatenated product of 192 and (1,2,3)
 
      The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and 5, giving the pandigital, 918273645, which is the concatenated product of 9 and (1,2,3,4,5).

      What is the largest 1 to 9 pandigital 9-digit number that can be formed as the concatenated product of an integer with (1,2, ... , n) where n > 1?"})
  (:use clojure.contrib.combinatorics
	tools.numbers
	clojure.test))

(set! *print-length* 103)
(set! *print-level* 15)

(deftest test-examples
  (is (= 9 (count (reduce concat (map digits (map #(* 192 %) '(1 2 3)))))))
  (is (= 9 (count (reduce concat (map digits (map #(* 9 %) '(1 2 3 4 5))))))))

(defn pandigital? [number]
  (let [d (digits number)]
    (and (= 9 (count (distinct d)))
	 (= 9 (count d)))))

(def perms  (map integer (permutations '(9 8 7 6 5 4 3 2 1))))
;; number of permutations is of course (factorial 9)

;; find a permutation, p that can be split in n pieces, where
(defn task [p m n]
  (and (= (digits p) 
	  (reduce concat (map digits (map #(* m %) (range 1 (+ n 1)))))) 
       (pandigital? p)))

(deftest test-task
  (is (task 192384576 192 3))
  (is (task 918273645 9 5)))

;; 918273645 is obviously not the correct answer so the answer is larger than that.
;; so (- 987654321 918273645) = 69380676 numbers to check, obviously too much to 
;; brute force, but not all are pandigital, which reduces the number of checks we 
;; have to do quite a bit:

;; (count (take-while #(< 918273645 %) perms))
;; 35899 

;; as the number of splits have to be more than one (n>1) the number of splits has 
;; to be 3
;; the number (map #(integer %) (partition 3 (digits 987654321)))

(defn check [[a b c]] (and (= (* 2 (integer a)) (integer b)) (= (* 3 (integer a)) (integer c))))
;;(filter check (map #(partition 3 (digits %)) (take-while #(< 918273645 %) perms)))
;; ()
;; so much for that train of thought

(def low-perms  (map integer (permutations '(1 2 3 4 5 6 7 8 9))))
;; (count  (take-while #(<= % 192384576) low-perms))
;;35378

;; (filter check (map #(partition 3 (digits %)) [192384576]))
;; (filter check (map #(partition 3 (digits %)) (take-while #(<= % 192384576 ) low-perms)))
;;(((1 9 2) (3 8 4) (5 7 6)))

;; (time (filter check (map #(partition 3 (digits %)) perms)))
;; (((3 2 7) (6 5 4) (9 8 1))  ((2 7 3) (5 4 6) (8 1 9))  ((2 1 9) (4 3 8) (6 5 7))  ((1 9 2) (3 8 4) (5 7 6)))

;
(def divs [100000000 10000000 1000000 100000 10000 1000 100 10 1])

(def perms  (map integer (permutations '(9 8 7 6 5 4 3 2 1))))
(def perms  (map integer (permutations '(9 8 7 6 5 4 3 2 1))))

;;(filter pandigital? (map #(+ (* % 1000000) (* 2 % 1000) (* 3 %)) (range 1 1000)))
;;(192384576 219438657 267534801 273546819 327654981 384769152 619237854 781562340 915830742 916832745)
(def divs [100000000 10000000 1000000 100000 10000 1000 100 10 1])
;          (10000             1000           100        10     1)

(filter pandigital? (map #(+ (* % 10000) (* 2 % 100) (* 3 %)) (range 1 100000)))
; 983742651

(filter pandigital? (map #(+ (* % 10000) (* 2 % 100) (* 3 %)) (range 1 100000)))

;; 2 cifre => 
(def d2 '(98 97 96 95 94 93 92 91 89 87 86 85 84 83 82 81 79 78 76 75 74 73 72 71 69 68 67 65 64 63 62 61 59 58 57 56 54 53 52 51 49 48 47 46 45 43 42 41 39 38 37 36 35 34 32 31 29 28 27 26 25 24 23 21 19 18 17 16 15 14 13 12))

;; 3 cifre =>
(def d3 '(987 986 985 984 983 982 981 978 976 975 974 973 972 971 968 967 965 964 963 962 961 958 957 956 954 953 952 951 948 947 946 945 943 942 941 938 937 936 935 934 932 931 928 927 926 925 924 923 921 918 917 916 915 914 913 912 897 896 895 894 893 892 891 879 876 875 874 873 872 871 869 867 865 864 863 862 861 859 857 856 854 853 852 851 849 847 846 845 843 842 841 839 837 836 835 834 832 831 829 827 826 825 824 823 821 819 817 816 815 814 813 812 798 796 795 794 793 792 791 789 786 785 784 783 782 781 769 768 765 764 763 762 761 759 758 756 754 753 752 751 749 748 746 745 743 742 741 739 738 736 735 734 732 731 729 728 726 725 724 723 721 719 718 716 715 714 713 712 698 697 695 694 693 692 691 689 687 685 684 683 682 681 679 678 675 674 673 672 671 659 658 657 654 653 652 651 649 648 647 645 643 642 641 639 638 637 635 634 632 631 629 628 627 625 624 623 621 619 618 617 615 614 613 612 598 597 596 594 593 592 591 589 587 586 584 583 582 581 579 578 576 574 573 572 571 569 568 567 564 563 562 561 549 548 547 546 543 542 541 539 538 537 536 534 532 531 529 528 527 526 524 523 521 519 518 517 516 514 513 512 498 497 496 495 493 492 491 489 487 486 485 483 482 481 479 478 476 475 473 472 471 469 468 467 465 463 462 461 459 458 457 456 453 452 451 439 438 437 436 435 432 431 429 428 427 426 425 423 421 419 418 417 416 415 413 412 398 397 396 395 394 392 391 389 387 386 385 384 382 381 379 378 376 375 374 372 371 369 368 367 365 364 362 361 359 358 357 356 354 352 351 349 348 347 346 345 342 341 329 328 327 326 325 324 321 319 318 317 316 315 314 312 298 297 296 295 294 293 291 289 287 286 285 284 283 281 279 278 276 275 274 273 271 269 268 267 265 264 263 261 259 258 257 256 254 253 251 249 248 247 246 245 243 241 239 238 237 236 235 234 231 219 218 217 216 215 214 213 198 197 196 195 194 193 192 189 187 186 185 184 183 182 179 178 176 175 174 173 172 169 168 167 165 164 163 162 159 158 157 156 154 153 152 149 148 147 146 145 143 142 139 138 137 136 135 134 132 129 128 127 126 125 124 123))

(defn task [p m n]
  (and (= (digits p) 
	  (reduce concat (map digits (map #(* m %) (range 1 (+ n 1)))))) 
       (pandigital? p)))

(defn check [n] 
  (filter #(task (integer %) (integer (take n %)) n) (permutations '(9 8 7 6 5 4 3 2 1))))
;; problem038> (check 3)
;; ((3 2 7 6 5 4 9 8 1) (2 7 3 5 4 6 8 1 9) (2 1 9 4 3 8 6 5 7) (1 9 2 3 8 4 5 7 6))
  
(def f
     (for [p [[3 2 7 6 5 4 9 8 1] [2 7 3 5 4 6 8 1 9]]]
       (for [n (range 1 9)] (list (integer p) n (integer (take n p))))))
(def g
     (for [p [[3 2 7 6 5 4 9 8 1] [2 7 3 5 4 6 8 1 9] [9 8 7 6 5 4 3 2 1]]
	   n (range 1 9)
	   :when (let [h (integer (take n p))
		       d (concat-numbers h (* h 2) (* h 3))] 
		   (different-digits? d))]

       (let [h (integer (take n p))
	     d (concat-numbers h (* h 2) (* h 3))] 
	 (list (integer p) n h d))))

;(reduce-while different-digits? concat-numbers  (map #(* 192 %) (range 1 10)))
;(reduce-while different-digits? concat-numbers (map #(* 192 %) (iterate inc 1)))

(defn reduce-while 
  ([pred f coll]
     (let [s (seq coll)]
;       (do (println (first s) (pred (first s))))
       (if (and s (pred (first s)))
	 (reduce-while pred f (first s) (next s))
	 (f))))
  ([pred f val coll]
     (let [s (seq coll)]
       (if (and s (pred (first s)))
         (if (chunked-seq? s)
           (recur pred
		  f 
                  (.reduce (chunk-first s) f val)
                  (chunk-next s))
           (recur pred f (f val (first s)) (next s)))
         val))))

(defn concat-numbers [& numbers]
  (integer (reduce concat (map #(digits %) numbers))))

(defn different-digits? [number]
  (let [d (digits number)]
    (= (count (distinct d)) (count d))))


;; take a permutation
;;  let each head partition be the current result (implicitly different digits)
;;  just take head partition concatenated with (* 2 head partition) 
;;    while the current result has different digits and current result <= 987654321
;;    multiply by n+1 and concatenate the result to the current result


(comment
  (defn multi-check []
    (loop [p (permutations '(9 8 7 6 5 4 3 2 1))
	   catch (transient [])]
      (if (empty? p)
	(persistent! catch)
	nil
	(let [head (integer (take ))])
	)
      )))