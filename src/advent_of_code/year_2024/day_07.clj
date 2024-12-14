(ns advent-of-code.year-2024.day-07
  (:require [ysera.test :refer [is= is is-not]]
            [clojure.math.combinatorics :as combo]))

(def input (slurp "src/advent_of_code/year_2024/inputs/day07.txt"))
(def test-input "190: 10 19\n3267: 81 40 27\n83: 17 5\n156: 15 6\n7290: 6 8 6 15\n161011: 16 10 13\n192: 17 8 14\n21037: 9 7 18 13\n292: 11 6 16 20")

(defn apply-operands
  {:test (fn []
           (is= (apply-operands [1 2 3] [* +])
                5))}
  [numbers operands]
  (loop [total (first numbers)
         numbers (rest numbers)
         operands operands]
    (if (empty? numbers)
      total
      (recur ((first operands) total (first numbers))
             (rest numbers)
             (rest operands)))))

(def get-operands-to-test
  (memoize
    (fn [possible-operands number-of-operands]
      (combo/selections possible-operands number-of-operands))))

(defn can-make-valid-equation?
  [test-value numbers possible-operands]
  (loop [operands-to-test (get-operands-to-test possible-operands (dec (count numbers)))]
    (cond
      (empty? operands-to-test)
      false

      (= test-value (apply-operands numbers (first operands-to-test)))
      true

      :else
      (recur (rest operands-to-test)))))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 3749))}
  [input]
  (->> (clojure.string/split-lines input)
       (map (fn [line]
              (->> (re-seq #"\d+" line)
                   (map read-string))))
       (keep (fn [[test-value & numbers]]
               (when (can-make-valid-equation? test-value numbers [+ *])
                 test-value)))
       (reduce +)))

(defn concatenation
  [x y]
  (read-string (str x y)))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input) 11387))}
  [input]
  (->> (clojure.string/split-lines input)
       (map (fn [line]
              (->> (re-seq #"\d+" line)
                   (map read-string))))
       (keep (fn [[test-value & numbers]]
               (when (can-make-valid-equation? test-value numbers [+ * concatenation])
                 test-value)))
       (reduce +)))

(comment
  ;; "Elapsed time: 170.270709 msecs"
  ;=> 1708857123053
  (time (part-1 input))

  ;; "Elapsed time: 24995.581166 msecs"
  ;=> 189207836795655
  (time (part-2 input))
  )
