(ns advent-of-code.year-2019.day-21
  (:require [ysera.test :refer [is= is is-not]]
            [advent-of-code.year-2019.intcode :refer [parse-program run-intcode-program]]))

(def input (slurp "src/advent_of_code/year_2019/inputs/day21.txt"))

(defn get-output-string
  [output]
  (let [outside-ascii-numbers (filter (fn [n] (> n 65535)) output)]
    (if-not (empty? outside-ascii-numbers)
      (->> outside-ascii-numbers
           (map str)
           (clojure.string/join " "))
      (->> output
           (map char)
           (apply str)))))

(defn print-output!
  [output]
  (-> (get-output-string output)
      (println)))

(defn part-1
  [input]
  (let [program (parse-program input)
        ;; Jump if ground 4 tiles away + any hole in-between
        commands ["NOT A J"
                  "NOT B T"
                  "OR T J"
                  "NOT C T"
                  "OR T J"
                  "AND D J"
                  "WALK"]]
    (-> (run-intcode-program program (map int (clojure.string/join "\n" (conj commands ""))))
        (:program-output)
        (print-output!))))

(defn part-2
  [input]
  (let [program (parse-program input)
        ;; Jump if ground 4 tiles away + any hole in-between + ground at either 5 or 8 tiles away
        commands ["NOT A J"
                  "NOT B T"
                  "OR T J"
                  "NOT C T"
                  "OR T J"
                  "AND D J"
                  "NOT E T"
                  "NOT T T"
                  "OR H T"
                  "AND T J"
                  "RUN"]]
    (-> (run-intcode-program program (map int (clojure.string/join "\n" (conj commands ""))))
        (:program-output)
        (print-output!))))

(comment
  (time (part-1 input))
  ;; 19347868
  ;"Elapsed time: 23.902417 msecs"
  ;=> nil

  (time (part-2 input))
  ;; 1142479667
  ;"Elapsed time: 343.425125 msecs"
  ;=> nil
  )
