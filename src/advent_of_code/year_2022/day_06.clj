(ns advent-of-code.year-2022.day-06
  (:require [ysera.test :refer [is= is is-not]]))

(def test-input "mjqjpqmgbljsphdztnvjfqwrcgsmlb")
(def input (slurp "src/advent_of_code/year_2022/inputs/day06.txt"))

(defn detect-start-of
  [input length]
  (reduce-kv (fn [buffer i letter]
               (if (= length (count (into #{} buffer)))
                 (reduced i)
                 (take length (conj buffer letter))))
             (list)
             (into [] (clojure.string/split input #""))))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 7))}
  [input]
  (detect-start-of input 4))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input) 19))}
  [input]
  (detect-start-of input 14))

(comment
  (time (part-1 input))
  ; 1361
  ;; "Elapsed time: 1.428125 msecs"

  (time (part-2 input))
  ; 3263
  ;; "Elapsed time: 6.325209 msecs"
  )
