(ns advent-of-code.year-2015.day-01
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2015/inputs/day01.txt"))

(defn part-1
  {:test (fn []
           (is= (part-1 "))(((((") 3))}
  [input]
  (reduce (fn [acc char]
            (case char
              \( (inc acc)
              \) (dec acc)
              acc))
          0
          input))

(defn part-2
  {:test (fn []
           (is= (part-2 "()())") 5))}
  [input]
  (loop [floor 0
         position 0]
    (if (= floor -1)
      position
      (let [c (nth input position)
            floor (if (= c \() (inc floor) (dec floor))]
        (recur floor (inc position)))))
  )

(comment
  (time (part-1 input))
  ;; "Elapsed time: 1.880541 msecs"
  ;=> 232

  (time (part-2 input))
  ;; "Elapsed time: 0.532834 msecs"
  ;=> 1783
  )
