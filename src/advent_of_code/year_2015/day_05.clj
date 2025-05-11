(ns advent-of-code.year-2015.day-05
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2015/inputs/day05.txt"))

(defn nice?
  {:test (fn []
           (is (nice? "ugknbfddgicrmopn"))
           (is (nice? "aaa"))
           (is-not (nice? "jchzalrnumimnmhp"))
           (is-not (nice? "haegwjzuvuyypxyu"))
           (is-not (nice? "dvszwmarrgswjxmb")))}
  [s]
  (and (<= 3 (count (re-seq #"[aeiou]" s)))
       (re-find #"(.)\1" s)
       (not (re-find #"ab|cd|pq|xy" s))))

(defn part-1
  [input]
  (->> (clojure.string/split-lines input)
       (filter nice?)
       (count)))

(defn nice-2?
  {:test (fn []
           (is (nice-2? "qjhvhtzxzqqjkmpb"))
           (is (nice-2? "xxyxx"))
           (is-not (nice-2? "aaa"))
           (is-not (nice-2? "uurcxstgmygtbstg"))
           (is-not (nice-2? "ieodomkazucvgmuy")))}
  [s]
  (and (re-find #"(..).*\1" s)
       (re-find #"(.).\1" s)))

(defn part-2
  [input]
  (->> (clojure.string/split-lines input)
       (filter nice-2?)
       (count)))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 4.333625 msecs"
  ;=> 255

  (time (part-2 input))
  ;; "Elapsed time: 1.720667 msecs"
  ;=> 55
  )
