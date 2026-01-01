(ns advent-of-code.year-2025.day-09
  (:require [ysera.test :refer [is= is is-not]]))

;; With Tomas

(def input (slurp "src/advent_of_code/year_2025/inputs/day09.txt"))
(def test-input "7,1\n11,1\n11,7\n9,7\n9,5\n2,5\n2,3\n7,3")

(defn parse-input
  [input]
  (->> (clojure.string/split-lines input)
       (mapv (fn [r] (mapv read-string (re-seq #"\d+" r))))))

(defn naive-calculation-of-largest-area
  {:test (fn []
           (is= (naive-calculation-of-largest-area (parse-input test-input))
                50))}
  [coordinates]
  (loop [[coordinate & coordinates] coordinates
         result 0]
    (if (empty? coordinates)
      result
      (recur coordinates
             (reduce (fn [result c]
                       (max result
                            (->> (map - coordinate c)
                                 (map abs)
                                 (map inc)
                                 (apply *))))
                     result
                     coordinates)))))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 50))}
  [input]
  (naive-calculation-of-largest-area (parse-input input)))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input) 24))}
  [input]
  )

(comment
  (time (part-1 input))
  ;; "Elapsed time: 79.347333 msecs"
  ;=> 4737096935

  (time (part-2 input))
  ;;
  )
