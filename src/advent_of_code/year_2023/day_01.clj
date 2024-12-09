(ns advent-of-code.year-2023.day-01
  (:require [ysera.test :refer [is= is is-not]]
            [ysera.collections :refer [index-of]]))

(def input (slurp "src/advent_of_code/year_2023/inputs/day01.txt"))
(def test-input "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet")
(def test-input-2 "two1nine\neightwothree\nabcone2threexyz\nxtwone3four\n4nineeightseven2\nzoneight234\n7pqrstsixteen")

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 142))}
  [input]
  (reduce (fn [a line]
            (let [numbers (->> line
                               (re-seq #"\d")
                               (map read-string))]
              (+ a (* 10 (first numbers)) (last numbers))))
          0
          (clojure.string/split-lines input)))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input-2) 281))}
  [input]
  (reduce (fn [a line]
            (let [numbers (->> line
                                (re-seq #"(?=(\d|one|two|three|four|five|six|seven|eight|nine))")
                                (map (fn [[_ d]]
                                       (if-let [i (index-of [nil "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"] d)]
                                         i
                                         (read-string d)))))]
              (+ a (* 10 (first numbers)) (last numbers))))
          0
          (clojure.string/split-lines input)))

(comment
  (time (part-1 input))
  ;; 54927
  ;; "Elapsed time: 3.118333 msecs"

  (time (part-2 input))
  ;; 54581
  ;; "Elapsed time: 13.659542 msecs"
  )

