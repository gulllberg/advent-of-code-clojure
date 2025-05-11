(ns advent-of-code.year-2015.day-02
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2015/inputs/day02.txt"))
(def test-input "2x3x4")

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 58))}
  [input]
  (->> (clojure.string/split-lines input)
       (map (fn [line]
              (->> (re-seq #"\d+" line)
                   (map read-string))))
       (reduce (fn [acc [l w h :as dims]]
                 (let [smallest-sides (take 2 (sort dims))
                       surface (* 2 l w h (+ (/ 1 l) (/ 1 w) (/ 1 h)))
                       slack (reduce * smallest-sides)]
                   (+ acc slack surface)))
               0)))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input) 34))}
  [input]
  (->> (clojure.string/split-lines input)
       (map (fn [line]
              (->> (re-seq #"\d+" line)
                   (map read-string))))
       (reduce (fn [acc dims]
                 (let [smallest-sides (take 2 (sort dims))
                       perimeter (* 2 (reduce + smallest-sides))
                       bow (reduce * dims)]
                   (+ acc perimeter bow)))
               0)))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 35.746 msecs"
  ;=> 1598415

  (time (part-2 input))
  ;; "Elapsed time: 5.766834 msecs"
  ;=> 3812909
  )
