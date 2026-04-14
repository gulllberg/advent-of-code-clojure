(ns advent-of-code.year-2019.day-05
  (:require [ysera.test :refer [is= is is-not]]
            [advent-of-code.year-2019.intcode :refer [parse-program run-intcode-program]]))

(def input (slurp "src/advent_of_code/year_2019/inputs/day05.txt"))

(defn part-1
  {:test (fn []
           (is= (part-1 input) 7692125))}
  [input]
  (-> (parse-program input)
      (run-intcode-program [1])
      (:program-output)
      (last)))

(defn part-2
  {:test (fn []
           (is= (part-2 input) 14340395))}
  [input]
  (-> (parse-program input)
      (run-intcode-program [5])
      (:program-output)
      (last)))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 0.958375 msecs"
  ;=> 7692125

  (time (part-2 input))
  ;; "Elapsed time: 1.757334 msecs"
  ;=> 14340395
  )
