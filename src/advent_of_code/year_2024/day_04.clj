(ns advent-of-code.year-2024.day-04
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2024/inputs/day04.txt"))
(def test-input "MMMSXXMASM\nMSAMXMSMSA\nAMXSXMAAMM\nMSAMASMSMX\nXMASAMXAMM\nXXAMMXXAMA\nSMSMSASXSS\nSAXAMASAAA\nMAMMMXMMMM\nMXMXAXMASX")

(defn parse-input
  [input]
  (let [lines (clojure.string/split-lines input)]
    (reduce (fn [a i]
              (let [line (nth lines i)]
                (reduce (fn [a j]
                          (let [c (nth line j)]
                            (assoc a [i j] c)))
                        a
                        (range (count line)))))
            {}
            (range (count lines)))))

(def all-directions (for [i (range -1 2)
                          j (range -1 2)
                          :when (not= [i j] [0 0])]
                      [i j]))

(defn vector-add
  [u v]
  (map + u v))

(defn vector-multiply
  [u s]
  (map (fn [x] (* x s)) u))

(defn get-number-of-xmas
  [grid position]
  (if (not= \X (get grid position))
    0
    (->> all-directions
         (filter (fn [d]
                   (and (= \M (get grid (vector-add position d)))
                        (= \A (get grid (vector-add position (vector-multiply d 2))))
                        (= \S (get grid (vector-add position (vector-multiply d 3)))))))
         (count))))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 18))}
  [input]
  (let [grid (parse-input input)]
    (->> (keys grid)
         (reduce (fn [a p]
                   (+ a (get-number-of-xmas grid p)))
                 0))))

(def diagonal-directions [[-1 -1] [-1 1] [1 -1] [1 1]])

(defn get-number-of-x-mas
  [grid position]
  (cond
    (not= \A (get grid position))
    0

    (= 2 (->> diagonal-directions
              (filter (fn [d]
                        (and (= \M (get grid (vector-add position (vector-multiply d -1))))
                             (= \S (get grid (vector-add position d))))))
              (count)))
    1

    :else
    0))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input) 9))}
  [input]
  (let [grid (parse-input input)]
    (->> (keys grid)
         (reduce (fn [a p]
                   (+ a (get-number-of-x-mas grid p)))
                 0))))

(comment
  ;; "Elapsed time: 58.0835 msecs"
  ;=> 2662
  (time (part-1 input))

  ;; "Elapsed time: 37.075917 msecs"
  ;=> 2034
  (time (part-2 input))
  )
