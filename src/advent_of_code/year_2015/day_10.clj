(ns advent-of-code.year-2015.day-10
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2015/inputs/day10.txt"))

;; Very slow (6 seconds part 1, 18 minutes part 2)
;(defn get-next
;  {:test (fn []
;           (is= (get-next "1") "11")
;           (is= (get-next "11") "21")
;           (is= (get-next "21") "1211")
;           (is= (get-next "1211") "111221")
;           (is= (get-next "111221") "312211"))}
;  [s]
;  (->> (re-seq #"(\d)\1*" s)
;       (reduce (fn [a [d-with-length d]]
;                 (str a (count d-with-length) d))
;               "")))

(defn next-look-and-say [s]
    {:test (fn []
             (is= (next-look-and-say "1") "11")
             (is= (next-look-and-say "11") "21")
             (is= (next-look-and-say "21") "1211")
             (is= (next-look-and-say "1211") "111221")
             (is= (next-look-and-say "111221") "312211"))}
  (apply str
         (mapcat (fn [group]
                   [(count group) (first group)])
                 (partition-by identity s))))

(defn nth-look-and-say [seed n]
  (nth (iterate next-look-and-say seed) n))

(defn part-1
  [input]
  (count (nth-look-and-say input 40)))

(defn part-2
  [input]
  (count (nth-look-and-say input 50)))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 307.034041 msecs"
  ;=> 329356

  (time (part-2 input))
  ;; "Elapsed time: 3726.994916 msecs"
  ;=> 4666278
  )
