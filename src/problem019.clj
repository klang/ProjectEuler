(ns problem019
  (meta {:description "You are given the following information, but you may prefer to do some research for yourself.

1 Jan 1900 was a Monday.
Thirty days has September,
April, June and November.
All the rest have thirty-one,
Saving February alone,
Which has twenty-eight, rain or shine.
And on leap years, twenty-nine.
A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.

How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?"})
  (:use clojure.test))


(defn leap-year [year]
  (if (zero? (rem year 4))
    (if (zero? (rem year 100))
      (if (zero? (rem year 400))
	true
	false)
      true)
    false))

(deftest test-leap-year
  (is (not (leap-year 1500)) "a century, that does not divide 400")
  (is (leap-year 1600) "a century, that divides 400")
  (is (not (leap-year 1900)) "a century, that does not divide 400")
  (is (leap-year 1972) "when my brother was born, leap year")
  (is (leap-year 2000) "a century, that divides 400")
  (is (leap-year 2004) "divides 4")
  )

;(run-tests)

;; why not try to use a bit of Java for a change?
(import java.util.Date java.util.Calendar java.util.GregorianCalendar)

;;   GregorianCalendar(int year, int month, int dayOfMonth) 
(.. (GregorianCalendar. 2010 0 1) (get Calendar/DAY_OF_WEEK))

(deftest test-gregorian
  (is (= (Calendar/MONDAY) 
	 (.. (GregorianCalendar. 1900 0 1) (get Calendar/DAY_OF_WEEK) ))))

;; this creates a lot of Calendar objects, but it gets the job done fast
(count (filter (fn [[year month]] 
		  (= (Calendar/SUNDAY) 
		     (.. (GregorianCalendar. year month 1) (get Calendar/DAY_OF_WEEK)))) 
		(for [year (range 1901 2001) month (range 12)] [year month])))
;; 171

