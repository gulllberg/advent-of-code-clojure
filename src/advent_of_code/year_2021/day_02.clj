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


(defn part-1
  []
  (let [[horizontal depth] (execute-instructions [0 0] (clojure.string/split-lines input))]
    (* horizontal depth)))

(comment
  (time (part-1))
  ; 1714950
  ; "Elapsed time: 1.336 msecs"
  )

(defn execute-instruction-2
  [[horizontal depth aim] instruction]
  (let [split-instruction (clojure.string/split instruction #" ")
        direction (first split-instruction)
        distance (read-string (second split-instruction))]
    (condp = direction
      "forward" [(+ horizontal distance) (+ depth (* aim distance)) aim]
      "up" [horizontal depth (- aim   distance)]
      "down" [horizontal depth (+ aim distance)]
      [horizontal depth aim])))

(defn execute-instructions-2
  {:test (fn []
           (is= (execute-instructions-2 [0 0 0] ["forward 5"
                                             "down 5"
                                             "forward 8"
                                             "up 3"
                                             "down 8"
                                             "forward 2"])
                [15 60 10]))}
  [position instructions]
  (reduce execute-instruction-2 position instructions))

(defn part-2
  []
  (let [[horizontal depth aim] (execute-instructions-2 [0 0 0] (clojure.string/split-lines input))]
    (* horizontal depth)))

(comment
  (time (part-2))
  ; 1281977850
  ; "Elapsed time: 1.26775 msecs"
  )
