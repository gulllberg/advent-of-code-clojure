(ns advent-of-code.year-2024.day-03
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2024/inputs/day03.txt"))
(def test-input "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 161))}
  [input]
  (->> (re-seq #"mul\((\d+),(\d+)\)" input)
       (map (fn [[_ d1 d2]]
              (* (read-string d1) (read-string d2))))
       (reduce +)))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input) 42))}
  [input]
  42)

(comment
  ;; "Elapsed time: 1.467208 msecs"
  ;=> 169021493
  (time (part-1 input))

  ;;
  (time (part-2 input))
  )
