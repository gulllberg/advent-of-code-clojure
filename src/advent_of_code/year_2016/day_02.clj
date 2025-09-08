(ns advent-of-code.year-2016.day-02
  (:require [ysera.test :refer [is= is is-not]]
            [advent-of-code.grid :refer [manhattan-distance]]))

(def input (slurp "src/advent_of_code/year_2016/inputs/day02.txt"))
(def test-input "ULL\nRRDDD\nLURDL\nUUUUD")

(def position->number {[-1 -1] "1"
                       [-1 0]  "2"
                       [-1 1]  "3"
                       [0 -1]  "4"
                       [0 0]   "5"
                       [0 1]   "6"
                       [1 -1]  "7"
                       [1 0]   "8"
                       [1 1]   "9"})

(def instruction->direction {\U [-1 0]
                             \D [1 0]
                             \L [0 -1]
                             \R [0 1]})

(defn clamp
  [n floor ceiling]
  (min (max n floor) ceiling))

(defn do-instruction
  [position instruction]
  (let [direction (instruction->direction instruction)
        position (mapv + position direction)]
    (into [] (map (fn [n]
                    (clamp n -1 1))
                  position))))

(defn do-line
  [position line]
  (reduce do-instruction position line))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) "1985"))}
  [input]
  (second (reduce (fn [[position res] line]
                    (let [position (do-line position line)]
                      [position (str res (position->number position))]))
                  [[0 0] ""]
                  (clojure.string/split-lines input))))

(def position->number-2 {[-2 0]  "1"
                         [-1 -1] "2"
                         [-1 0]  "3"
                         [-1 1]  "4"
                         [0 -2]  "5"
                         [0 -1]  "6"
                         [0 0]   "7"
                         [0 1]   "8"
                         [0 2]   "9"
                         [1 -1]   "A"
                         [1 0]   "B"
                         [1 1]   "C"
                         [2 0]   "D"})

(defn valid-position?
  [position]
  (<= (manhattan-distance position [0 0]) 2))

(defn do-instruction-2
  [position instruction]
  (let [direction (instruction->direction instruction)
        new-position (mapv + position direction)]
    (if (valid-position? new-position)
      new-position
      position)))

(defn do-line-2
  [position line]
  (reduce do-instruction-2 position line))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input) "5DB3"))}
  [input]
  (second (reduce (fn [[position res] line]
                    (let [position (do-line-2 position line)]
                      [position (str res (position->number-2 position))]))
                  [[0 -2] ""]
                  (clojure.string/split-lines input))))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 6.306333 msecs"
  ;=> "74921"

  (time (part-2 input))
  ;; "Elapsed time: 3.465792 msecs"
  ;=> "A6B35"
  )
