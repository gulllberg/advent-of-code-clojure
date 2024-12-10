(ns advent-of-code.year-2022.day-01
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2022/inputs/day01.txt"))
(def test-input "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000")

(defn get-sum-of-top-n
  [input n]
  (->> (clojure.string/split input #"\n\n")
       (map (fn [elf]
              (->> (clojure.string/split-lines elf)
                   (map read-string)
                   (reduce +))))
       (sort)
       (reverse)
       (take n)
       (reduce +)))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 24000))}
  [input]
  (get-sum-of-top-n input 1))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input) 45000))}
  [input]
  (get-sum-of-top-n input 3))

(comment
  (time (part-1 input))
  ; 71780
  ;; "Elapsed time: 2.413709 msecs"

  (time (part-2 input))
  ; 212489
  ;; "Elapsed time: 1.398041 msecs"
  )
