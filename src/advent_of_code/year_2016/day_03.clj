(ns advent-of-code.year-2016.day-03
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2016/inputs/day03.txt"))

(defn possible-triangle?
  {:test (fn []
           (is-not (possible-triangle? [5 10 25]))
           (is (possible-triangle? [5 10 12])))}
  [sides]
  (let [[s1 s2 s3] (sort sides)]
    (> (+ s1 s2) s3)))

(defn part-1
  [input]
  (reduce (fn [a line]
            (if (possible-triangle? (map read-string (re-seq #"\d+" line)))
              (inc a)
              a))
          0
          (clojure.string/split-lines input)))

(defn part-2
  [input]
  (reduce (fn [a three-lines]
            (let [lines (map (fn [line] (map read-string (re-seq #"\d+" line))) three-lines)]
              (+ a (count (filter possible-triangle? (apply mapv vector lines))))))
          0
          (partition 3 (clojure.string/split-lines input))))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 4.960459 msecs"
  ;=> 917

  (time (part-2 input))
  ;; "Elapsed time: 7.181375 msecs"
  ;=> 1649
  )
