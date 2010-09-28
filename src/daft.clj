(ns daft
  (meta {:description "The song 'Around the World' by Daft Punk has a deeply repetitive nature, unfortunately the lyrics is oftent stated wrong.
The song has 4 verses consisting of 8, 16, 28 and 20 double repetitions of the phrase 'Around the world'.
All in all, the song has 72 double repetitions or 144 repetitions of the iconic phrase."}))

(def lyrics (repeat "Around the world, around the world"))

(defn sing [lyrics]
 (reduce str
	 (interleave
	  (map #(reduce str %)
	       (map #(interleave % (repeat "\n"))
		    (map #(take % lyrics) [8 16 28 20])))
	  (repeat "\n") )))
(comment
  ;; works in the repl
  (println (sing song)))
