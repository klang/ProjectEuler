;; http://nakkaya.com/2010/04/31/yet-another-brute-force-sudoku-solver/

(ns sudoku.core
   (:use clojure.set)
   (:use clojure.contrib.seq-utils))

;; I know, there are more than a thousand sudoku solvers out there, but I've been meaning to learn the puzzle for a while now, on the other hand I have no interest in solving one for real so I hacked together the following snippet to brute force it for me.

(defn constraints [s i]
  (let [s (partition 9 s) r (/ i 9) c (mod i 9) gc (/ c 3) gr (/ r 3)
	every-nth (fn [s i] (map #(nth % i) s))
	grp-col (every-nth (map #(partition 3 %) s) gc)
	grp (take 3 (drop (* 3 (int gr)) grp-col))]
    (into #{} (flatten [(nth s r) (every-nth s c) grp]))))

;; Rules are simple, when choosing a number, you can't use numbers that are already present in the same column, same row, and the same group, the set of these numbers will contain invalid choices for that position.

(defn solve [s]
   (if (.contains s 0)
     (let [i (.indexOf s 0)
           inject #(concat (take %2 %1) [%3] (drop (inc %2) %1))]
       (flatten (map #(solve (inject s i %))
                     (difference #{1 2 3 4 5 6 7 8 9} (constraints s i))))) 
     s))

;; For every empty position in the puzzle, we calculate the set of invalid choices, difference between the invalid choices and the set of numbers from 1 to 9 gives us the possible choices for that location, we inject each possible choice into this location, call solve on these new sudokus until a solution is found.

(comment
sudoku.core> (time (solve [0 0 3 0 2 0 6 0 0 9 0 0 3 0 5 0 0 1 0 0 1 8 0 6 4 0 0 0 0 8 1 0 2 9 0 0 7 0 0 0 0 0 0 0 8 0 0 6 7 0 8 2 0 0 0 0 2 6 0 9 5 0 0 8 0 0 2 0 3 0 0 9 0 0 5 0 1 0 3 0 0]))
;;"Elapsed time: 667.121581 msecs"
;;(4 8 3 9 2 1 6 5 7 9 6 7 3 4 5 8 2 1 2 5 1 8 7 6 4 9 3 5 4 8 1 3 2 9 7 6 7 2 9 5 6 4 1 3 8 1 3 6 7 9 8 2 4 5 3 7 2 6 8 9 5 1 4 8 1 4 2 5 3 7 6 9 6 9 5 4 1 7 3 8 2)

sudoku.core> (time (solve [2 0 0 0 8 0 3 0 0 0 6 0 0 7 0 0 8 4 0 3 0 5 0 0 2 0 9 0 0 0 1 0 5 4 0 8 0 0 0 0 0 0 0 0 0 4 0 2 7 0 6 0 0 0 3 0 1 0 0 7 0 4 0 7 2 0 0 4 0 0 6 0 0 0 4 0 1 0 0 0 3]))
;; "Elapsed time: 1494.908312 msecs"
;; (2 4 5 9 8 1 3 7 6 1 6 9 2 7 3 5 8 4 8 3 7 5 6 4 2 1 9 9 7 6 1 2 5 4 3 8 5 1 3 4 9 8 6 2 7 4 8 2 7 3 6 9 5 1 3 9 1 6 5 7 8 4 2 7 2 8 3 4 9 1 6 5 6 5 4 8 1 2 7 9 3)
)