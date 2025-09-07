(ns advent-of-code.year-2016.day-01
  (:require [ysera.test :refer [is= is is-not]]
            [advent-of-code.grid :refer [turn-left turn-right manhattan-distance]]))

(def input (slurp "src/advent_of_code/year_2016/inputs/day01.txt"))
(def test-input "R2, R2, R2")
(def test-input-2 "R8, R4, R4, R8")

(defn vector-multiply
  [u s]
  (map (fn [x] (* x s)) u))

(defn follow-instructions
  [instructions]
  (reduce (fn [[position direction] instruction]
            (let [[_ rotation steps] (re-find #"(R|L)(\d+)" instruction)
                  rotation-fn (if (= rotation "R") turn-right turn-left)
                  direction (rotation-fn direction)
                  move (vector-multiply direction (read-string steps))
                  position (mapv + position move)]
              [position direction]))
          [[0 0] [0 1]]
          instructions))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 2))}
  [input]
  (let [[position _] (follow-instructions (clojure.string/split input #", "))]
    (manhattan-distance position [0 0])))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input-2) 4))}
  [input]
  (let [instructions (clojure.string/split input #", ")]
    (loop [visited #{[0 0]}
           position [0 0]
           direction [0 1]
           instructions instructions
           steps 0]
      (if (zero? steps)
        ;; Get new direction and rotate
        (let [instruction (first instructions)
              [_ rotation steps] (re-find #"(R|L)(\d+)" instruction)
              rotation-fn (if (= rotation "R") turn-right turn-left)
              direction (rotation-fn direction)]
          (recur visited
                 position
                 direction
                 (rest instructions)
                 (read-string steps)))
        ;; Walk a step in the current direction
        (let [position (mapv + position direction)]
          (if (contains? visited position)
            (manhattan-distance position [0 0])
            (recur (conj visited position)
                   position
                   direction
                   instructions
                   (dec steps))))))))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 4.205459 msecs"
  ;=> 279

  (time (part-2 input))
  ;; "Elapsed time: 1.974833 msecs"
  ;=> 163
  )
