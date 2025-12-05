(ns advent-of-code.year-2025.day-04
  (:require [ysera.test :refer [is= is is-not]]
            [advent-of-code.grid :refer [parse-grid
                                         directions-with-diagonals
                                         get-neighbours]]))

(def input (slurp "src/advent_of_code/year_2025/inputs/day04.txt"))
(def test-input "..@@.@@@@.\n@@@.@.@.@@\n@@@@@.@.@@\n@.@@@@..@.\n@@.@@@@.@@\n.@@@@@@@.@\n.@.@.@.@@@\n@.@@@.@@@@\n.@@@@@@@@.\n@.@.@@@.@.")

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 13))}
  [input]
  (let [grid (parse-grid input)]
    (reduce-kv (fn [a position c]
                 (if (and (= c \@)
                          (->> (get-neighbours position directions-with-diagonals)
                               (filter (fn [p]
                                         (= (get grid p) \@)))
                               (count)
                               (> 4)))
                   (inc a)
                   a))
               0
               grid)))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input) 43))}
  [input]
  (let [grid (parse-grid input)]
    (loop [paper-positions (->> grid
                                (keys)
                                (filter (fn [p] (= (get grid p) \@)))
                                (into #{}))
           positions-to-check paper-positions]
      (if (empty? positions-to-check)
        (- (count (re-seq #"@" input)) (count paper-positions))
        (let [[next-paper-positions potential-positions-to-check] (reduce (fn [[next-paper-positions potential-positions-to-check] position]
                                                                       (let [paper-neighbours (->> (get-neighbours position directions-with-diagonals)
                                                                                                   (filter (fn [p]
                                                                                                             (contains? paper-positions p))))]
                                                                         (if (->> paper-neighbours
                                                                                  (count)
                                                                                  (> 4))
                                                                           [(disj next-paper-positions position) (reduce conj potential-positions-to-check paper-neighbours)]
                                                                           [next-paper-positions potential-positions-to-check])))
                                                                     [paper-positions #{}]
                                                                     positions-to-check)]
          (recur next-paper-positions (clojure.set/intersection next-paper-positions potential-positions-to-check)))))))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 106.054625 msecs"
  ;  => 1493

  (time (part-2 input))
  ;; "Elapsed time: 174.924458 msecs"
  ;=> 9194
  )
