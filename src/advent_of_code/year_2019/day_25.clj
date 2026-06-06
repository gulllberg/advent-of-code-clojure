(ns advent-of-code.year-2019.day-25
  (:require [ysera.test :refer [is= is is-not]]
            [advent-of-code.year-2019.intcode :refer [parse-program run-intcode-program]]))

;; With Tomas

(def input (slurp "src/advent_of_code/year_2019/inputs/day25.txt"))

(defn get-output-string
  [output]
  (->> output
       (map char)
       (apply str)))

(defn print-output!
  [output]
  (-> (get-output-string output)
      (println)))

(def commands ["inv"
               "north"
               ;; Light
               ;"take festive hat"
               "east"
               ;"take prime number"
               "west"
               "west"
               ;; Light
               "take sand"
               "south"
               "north"
               "east"
               "south"
               ;; Back at start
               "east"
               "north"
               ;; Heavy
               ;"take weather machine"
               "north"
               ;; Heavy
               ;"take mug"
               "south"
               "south"
               "east"
               "north"
               "east"
               ;; End game
               ;"take escape pod"
               "east"
               ;; Light
               "take astronaut ice cream"
               "west"
               "west"
               "south"
               "west"
               "west"
               ;; Back at start
               "south"
               "south"
               ;; Light
               "take mutex"
               "south"
               ;; Light
               "take boulder"
               "east"
               ;; Can't move
               ;"take giant electromagnet"
               "south"
               "inv"
               "east"
               ])

(defn part-1
  [input]
  (let [program (parse-program input)]
    (-> (run-intcode-program program (map int (clojure.string/join "\n" (conj commands ""))))
        (:program-output)
        (print-output!))))

(comment
  (time (part-1 input))
  ;; 2236672
  )
