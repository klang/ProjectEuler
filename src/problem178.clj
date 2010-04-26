(ns problem178
  #^{:description "Consider the number 45656. 
It can be seen that each pair of consecutive digits of 45656 has a difference of one.
A number for which every pair of consecutive digits has a difference of one is called a step number.
A pandigital number contains every decimal digit from 0 to 9 at least once.
How many pandigital step numbers less than 10^40 are there?"
     :idea "The range is simply too big to check every number for the step-number poperty. 
There must be a way to generate/count 40 character strings in the desired way.
To make things managable, split the count in strings with 2,3,4,...10 characters." 
     }
  (:use clojure.contrib.combinatorics))

