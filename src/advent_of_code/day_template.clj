(ns advent-of-code.day-template
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2025/inputs/day01.txt"))
(def test-input "")

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 42))}
  [input]
  42)

(defn part-2
  {:test (fn []
           (is= (part-2 test-input) 42))}
  [input]
  42)

(comment
  (time (part-1 input))
  ;;

  (time (part-2 input))
  ;;
  )
