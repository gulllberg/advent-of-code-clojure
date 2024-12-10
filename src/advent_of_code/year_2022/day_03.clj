(ns advent-of-code.year-2022.day-03
  (:require [ysera.test :refer [is= is is-not]]))

(def test-input "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw")
(def input (slurp "src/advent_of_code/year_2022/inputs/day03.txt"))

(def priorities (reduce-kv (fn [a i v]
                             (assoc a v i))
                           {}
                           (into [] (map identity "0abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 157))}
  [input]
  (reduce (fn [a line]
            (let [[first-half second-half] (map set (split-at (/ (count line) 2) line))]
              (reduce + a (map priorities
                               (clojure.set/intersection first-half second-half)))))
          0
          (clojure.string/split-lines input)))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input) 70))}
  [input]
  (reduce (fn [a group]
            (+ a (-> (apply clojure.set/intersection (map (fn [line]
                                                            (set (map identity line)))
                                                          group))
                     (first)
                     (priorities))))
          0
          (partition 3 (clojure.string/split-lines input))))

(comment
  (time (part-1 input))
  ; 7811
  ;; "Elapsed time: 9.949458 msecs"

  (time (part-2 input))
  ; 2639
  ;; "Elapsed time: 2.099791 msecs"
  )
