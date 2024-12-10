(ns advent-of-code.year-2022.day-04
  (:require [ysera.test :refer [is= is is-not]]))

(def test-input "2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8")
(def input (slurp "src/advent_of_code/year_2022/inputs/day04.txt"))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 2))}
  [input]
  (reduce (fn [a line]
            (let [[elf-1 elf-2] (->> line
                                     (re-seq #"\d+")
                                     (map read-string)
                                     (partition 2))]
              ;; Complete overlap
              (if (or (and (>= (first elf-1) (first elf-2))
                           (<= (second elf-1) (second elf-2)))
                      (and (>= (first elf-2) (first elf-1))
                           (<= (second elf-2) (second elf-1))))
                (inc a)
                a)))
          0
          (clojure.string/split-lines input)))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input) 4))}
  [input]
  (reduce (fn [a line]
            (let [[elf-1 elf-2] (->> line
                                     (re-seq #"\d+")
                                     (map read-string)
                                     (partition 2))]
              ;; Some overlap
              (if (or (and (>= (second elf-1) (first elf-2))
                           (<= (first elf-1) (second elf-2)))
                      (and (<= (first elf-1) (second elf-2))
                           (>= (second elf-1) (first elf-2))))
                (inc a)
                a)))
          0
          (clojure.string/split-lines input)))

(comment
  (time (part-1 input))
  ; 530
  ;; "Elapsed time: 3.839083 msecs"

  (time (part-2 input))
  ; 903
  ;; "Elapsed time: 5.231625 msecs"
  )
