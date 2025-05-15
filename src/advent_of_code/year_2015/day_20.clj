(ns advent-of-code.year-2015.day-20
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2015/inputs/day20.txt"))

(defn factors
  {:test (fn []
           (is= (factors 4) #{1 2 4})
           (is= (factors 6) #{1 2 3 6})
           (is= (factors 12) #{1 2 3 4 6 12}))}
  [n]
  (->> (range 1 (inc (Math/sqrt n)))
       (mapcat (fn [i]
                 (if (zero? (mod n i))
                   [i (/ n i)]
                   [])))
       (set)))

(defn get-presents-at-house
  {:test (fn []
           (is= (get-presents-at-house 4) 70))}
  [n]
  (* 10 (reduce + (factors n))))

(defn part-1
  {:test (fn []
           (is= (part-1 "100") 6))}
  [input]
  (let [target-presents (read-string input)]
    (->> (range)
         (map (fn [n]
                [n (get-presents-at-house n)]))
         (filter (fn [[_ presents]]
                   (>= presents target-presents)))
         (ffirst))))

(defn get-presents-at-house-2
  [n]
  (->> (factors n)
       (remove (fn [f] (> n (* 50 f))))
       (reduce +)
       (* 11)))

(defn part-2
  [input]
  (let [target-presents (read-string input)]
    (->> (range)
         (map (fn [n]
                [n (get-presents-at-house-2 n)]))
         (filter (fn [[_ presents]]
                   (>= presents target-presents)))
         (ffirst))))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 23355.590166 msecs"
  ;=> 665280

  (time (part-2 input))
  ;; "Elapsed time: 27484.793041 msecs"
  ;=> 705600
  )