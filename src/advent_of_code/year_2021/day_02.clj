(ns advent-of-code.year-2021.day-02
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2021/inputs/day02.txt"))

(defn execute-instruction
  [[horizontal depth] instruction]
  (let [split-instruction (clojure.string/split instruction #" ")
        direction (first split-instruction)
        distance (read-string (second split-instruction))]
    (condp = direction
      "forward" [(+ horizontal distance) depth]
      "up" [horizontal (- depth distance)]
      "down" [horizontal (+ depth distance)]
      [horizontal depth])))

(defn execute-instructions
  {:test (fn []
           (is= (execute-instructions [0 0] ["forward 5"
                                             "down 5"
                                             "forward 8"
                                             "up 3"
                                             "down 8"
                                             "forward 2"])
                [15 10]))}
  [position instructions]
  (reduce execute-instruction position instructions))


(defn solve-a
  []
  (let [[horizontal depth] (execute-instructions [0 0] (clojure.string/split-lines input))]
    (* horizontal depth)))

(comment
  (solve-a)
  ; 1714950
  )

(defn execute-instruction-b
  [[horizontal depth aim] instruction]
  (let [split-instruction (clojure.string/split instruction #" ")
        direction (first split-instruction)
        distance (read-string (second split-instruction))]
    (condp = direction
      "forward" [(+ horizontal distance) (+ depth (* aim distance)) aim]
      "up" [horizontal depth (- aim   distance)]
      "down" [horizontal depth (+ aim distance)]
      [horizontal depth aim])))

(defn execute-instructions-b
  {:test (fn []
           (is= (execute-instructions-b [0 0 0] ["forward 5"
                                             "down 5"
                                             "forward 8"
                                             "up 3"
                                             "down 8"
                                             "forward 2"])
                [15 60 10]))}
  [position instructions]
  (reduce execute-instruction-b position instructions))

(defn solve-b
  []
  (let [[horizontal depth aim] (execute-instructions-b [0 0 0] (clojure.string/split-lines input))]
    (* horizontal depth)))

(comment
  (solve-b)
  ; 1281977850
  )
