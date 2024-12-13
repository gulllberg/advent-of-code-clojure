(ns advent-of-code.year-2017.day-05
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2017/inputs/day05.txt"))

(defn part-1
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

(defn part-2
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
  (time (part-1 input))
  ;; 391540
  ;; "Elapsed time: 59.748416 msecs"

  (time (part-2 input))
  ;; 30513679
  ;; "Elapsed time: 2721.312583 msecs"
  )