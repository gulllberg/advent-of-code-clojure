(ns advent-of-code.year-2021.day-07
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2021/inputs/day07.txt"))
(def test-input [16 1 2 0 4 2 7 1 2 14])

(defn mean
  [numbers]
  (/ (apply + numbers) (count numbers)))

(defn median
  [numbers]
  (let [sorted-numbers (sort numbers)
        cnt (count numbers)]
    (if (odd? cnt)
      (nth sorted-numbers (/ (dec cnt) 2))
      (Math/round (float (mean [(nth sorted-numbers (dec (/ cnt 2)))
                                (nth sorted-numbers (/ cnt 2))]))))))

(defn get-fuel-consumption
  [numbers position]
  (reduce (fn [s n]
            (+ s (Math/abs (- n position))))
          0
          numbers))

(defn part-1
  []
  (let [numbers (map read-string (clojure.string/split input #","))
        position (median numbers)]
    (get-fuel-consumption numbers position)))

(comment
  (time (part-1))
  ; 336120
  ; "Elapsed time: 21.551417 msecs"
  )

(defn get-fuel-consumption-2
  [numbers position]
  (reduce (fn [s n]
            (apply + s (range 1 (inc (Math/abs (- n position))))))
          0
          numbers))

(defn part-2
  []
  (let [numbers (map read-string (clojure.string/split input #","))
        position (Math/round (float (mean numbers)))]
    ;; THIS DOES NOT ACTUALLY SOLVE THE PROBLEM, POSITION IS OFF BY ONE
    (println position)
    (get-fuel-consumption-2 numbers position)))

(comment
  (time (part-2))
  ; TOO HIGH 96864332
  ; "Elapsed time: 20.160917 msecs"
  (mean (map read-string (clojure.string/split input #","))) ;; 462.508
  (get-fuel-consumption-2 (map read-string (clojure.string/split input #",")) 462) ;; 96864235 CORRECT
  ;; The mean minimises d^2 and not d * (d + 1) as is the formula in this case.
  ;; The mean is however a good approximation, but off by one in my case. 462.508 needed to be rounded down.
  )
