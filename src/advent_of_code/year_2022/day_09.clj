(ns advent-of-code.year-2022.day-09
  (:require [ysera.test :refer [is= is is-not]]))

(def test-input "R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2")
(def input (slurp "src/advent_of_code/year_2022/inputs/day09.txt"))

(defn get-new-tail-position
  [[h-i h-j] [t-i t-j]]
  (let [distance-horizontal  (- h-j t-j)
        distance-vertical  (- h-i t-i)
        abs-distance-horizontal (abs distance-horizontal)
        abs-distance-vertical (abs distance-vertical)
        should-move (or (> abs-distance-horizontal 1) (> abs-distance-vertical 1))
        should-move-diagonally (and should-move (> abs-distance-horizontal 0) (> abs-distance-vertical 0))]
    (cond
      should-move-diagonally
      [(+ t-i (/ distance-vertical abs-distance-vertical)) (+ t-j (/ distance-horizontal abs-distance-horizontal))]

      ;; up/down
      (and should-move (> abs-distance-vertical 1))
      [(+ t-i (/ distance-vertical abs-distance-vertical)) t-j]

      ;; left/right
      should-move
      [t-i (+ t-j (/ distance-horizontal abs-distance-horizontal))]

      :else
      [t-i t-j])))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 13))}
  [input]
  (->> (clojure.string/split-lines input)
       (reduce (fn [[visited head-position tail-position] line]
                 (let [direction (subs line 0 1)
                       distance (read-string (subs line 2))
                       head-change (condp = direction
                                     "U" [1 0]
                                     "D" [-1 0]
                                     "L" [0 -1]
                                     "R" [0 1])]
                   (reduce (fn [[visited head-position tail-position] _]
                             (let [new-head-position (mapv + head-position head-change)
                                   new-tail-position (get-new-tail-position new-head-position tail-position)]
                               [(conj visited new-tail-position) new-head-position new-tail-position]))
                           [visited head-position tail-position]
                           (range distance))))
               [#{[0 0]} [0 0] [0 0]])
       (first)
       (count)))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input) 1)
           (is= (part-2 "R 5\nU 8\nL 8\nD 3\nR 17\nD 10\nL 25\nU 20") 36))}
  [input]
  (->> (clojure.string/split-lines input)
       (reduce (fn [[visited positions] line]
                 (let [direction (subs line 0 1)
                       distance (read-string (subs line 2))
                       head-change (condp = direction
                                     "U" [1 0]
                                     "D" [-1 0]
                                     "L" [0 -1]
                                     "R" [0 1])]
                   (reduce (fn [[visited positions] _]
                             (reduce (fn [[visited positions] i]
                                       (let [position (get positions i)]
                                         (condp = i
                                           ; head
                                           0 [visited (assoc positions 0 (mapv + position head-change))]

                                           ; tail
                                           9 (let [new-tail-position (get-new-tail-position (get positions 8) position)]
                                               [(conj visited new-tail-position) (assoc positions 9 new-tail-position)])

                                           ; middle
                                           (let [new-position (get-new-tail-position (get positions (dec i)) position)]
                                             [visited (assoc positions i new-position)]))))
                                     [visited positions]
                                     (range 10)))
                           [visited positions]
                           (range distance))))
               [#{[0 0]} (into [] (repeat 10 [0 0]))])
       (first)
       (count)))

(comment
  (time (part-1 input))
  ; 5513
  ;; "Elapsed time: 9.886292 msecs

  (time (part-2 input))
  ; 2427
  ;; "Elapsed time: 31.45475 msecs"
  )

