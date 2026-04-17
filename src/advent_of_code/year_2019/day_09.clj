(ns advent-of-code.year-2019.day-09
  (:require [ysera.test :refer [is= is is-not]]
            [advent-of-code.year-2019.intcode :refer [parse-program run-intcode-program]]))

(def input (slurp "src/advent_of_code/year_2019/inputs/day09.txt"))

(defn part-1
  [input]
  (-> (parse-program input)
      (run-intcode-program [1])
      (:program-output)
      (first)))

(defn part-2
  [input]
  (-> (parse-program input)
      (run-intcode-program [2])
      (:program-output)
      (first)))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 10.142458 msecs"
  ;=> 3780860499

  (time (part-2 input))
  ;; "Elapsed time: 267.4395 msecs"
  ;=> 33343
  )
