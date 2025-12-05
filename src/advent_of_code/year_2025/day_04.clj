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
                                         (= (get grid p nil) \@)))
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
  (loop [grid (parse-grid input)]
    (let [next-grid (reduce-kv (fn [next-grid position c]
                                 (if (and (= c \@)
                                          (->> (get-neighbours position directions-with-diagonals)
                                               (filter (fn [p]
                                                         (= (get grid p nil) \@)))
                                               (count)
                                               (<= 4)))
                                   (assoc next-grid position c)
                                   next-grid))
                               {}
                               grid)]
      (if (= grid next-grid)
        (- (count (re-seq #"@" input)) (count (keys grid)))
        (recur next-grid)))))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 106.054625 msecs"
  ;  => 1493

  (time (part-2 input))
  ;; "Elapsed time: 1026.859291 msecs"
  ;=> 9194
  )
