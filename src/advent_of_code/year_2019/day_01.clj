(ns advent-of-code.year-2019.day-01
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2019/inputs/day01.txt"))

(defn get-module-fuel-requirement
  {:test (fn []
           (is= (get-module-fuel-requirement 100756) 33583))}
  [mass]
  (-> (quot mass 3)
      (- 2)))

(defn part-1
  [input]
  (->> (clojure.string/split-lines input)
       (map read-string)
       (reduce (fn [a mass]
                 (+ a (get-module-fuel-requirement mass)))
               0)))

(defn get-module-fuel-requirement-recursively
  {:test (fn []
           (is= (get-module-fuel-requirement-recursively 100756) 50346))}
  [mass]
  (let [fuel-required (get-module-fuel-requirement mass)]
    (if (<= fuel-required 0)
      0
      (+ fuel-required (get-module-fuel-requirement-recursively fuel-required)))))

(defn part-2
  [input]
  (->> (clojure.string/split-lines input)
       (map read-string)
       (reduce (fn [a mass]
                 (+ a (get-module-fuel-requirement-recursively mass)))
               0)))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 0.516916 msecs"
  ;=> 3406342

  (time (part-2 input))
  ;; "Elapsed time: 0.613417 msecs"
  ;=> 5106629
  )
