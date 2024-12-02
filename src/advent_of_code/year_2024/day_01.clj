(ns advent-of-code.year-2024.day-01
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2024/inputs/day01.txt"))
(def test-input "3   4\n4   3\n2   5\n1   3\n3   9\n3   3")

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 11))}
  [input]
  (let [[list1 list2] (->> (clojure.string/split-lines input)
                           (reduce (fn [[a1 a2] line]
                                     (let [[v1 v2] (->> (re-seq #"\d+" line)
                                                        (map read-string))]
                                       [(conj a1 v1) (conj a2 v2)]))
                                   [[] []])
                           (map sort))]
    (->> (map (comp abs -) list1 list2)
         (reduce +))))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input) 31))}
  [input]
  (let [[list1 list2] (->> (clojure.string/split-lines input)
                           (reduce (fn [[a1 a2] line]
                                     (let [[v1 v2] (->> (re-seq #"\d+" line)
                                                        (map read-string))]
                                       [(conj a1 v1) (conj a2 v2)]))
                                   [[] []])
                           (map sort))
        freqs (frequencies list2)]
    (reduce (fn [a v]
              (+ a (* v (get freqs v 0))))
            0
            list1)))

(comment
  ;; "Elapsed time: 7.462084 msecs"
  ;=> 1660292
  (time (part-1 input))

  ;; "Elapsed time: 3.596292 msecs"
  ;=> 22776016
  (time (part-2 input))
  )
