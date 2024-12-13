(ns advent-of-code.year-2017.day-05
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2017/inputs/day05.txt"))

(defn problem-5a
  [input]
  (loop [list (mapv read-string (clojure.string/split input #"\n"))
         index 0
         number-of-jumps 0]
    (if (or (> 0 index)
            (<= (count list) index))
      number-of-jumps
      (recur (update list index inc)
             (+ index (get list index))
             (inc number-of-jumps)))))

(defn problem-5b
  [input]
  (loop [list (mapv read-string (clojure.string/split input #"\n"))
         index 0
         number-of-jumps 0]
    (if (or (> 0 index)
            (<= (count list) index))
      number-of-jumps
      (recur (update list index (fn [offset]
                                  (if (<= 3 offset)
                                    (dec offset)
                                    (inc offset))))
             (+ index (get list index))
             (inc number-of-jumps)))))

(comment
  (problem-5a input)
  ;; 391540
  (problem-5b input)
  ;; 30513679
  )