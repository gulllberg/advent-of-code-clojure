(ns advent-of-code.year-2022.day-02
  (:require [ysera.test :refer [is= is is-not]]))

(def test-input "A Y\nB X\nC Z")
(def input (slurp "src/advent_of_code/year_2022/inputs/day02.txt"))

(defn get-result-score
  [me opp]
  (condp = me
    "A" (condp = opp
          "A" 3
          "B" 0
          "C" 6)
    "B" (condp = opp
          "A" 6
          "B" 3
          "C" 0)
    "C" (condp = opp
          "A" 0
          "B" 6
          "C" 3)))

(defn get-choice-score
  [me]
  (condp = me
    "A" 1
    "B" 2
    "C" 3))

(defn get-me-1
  [choice]
  (condp = choice
    "X" "A"
    "Y" "B"
    "Z" "C"))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 15))}
  [input]
  (reduce (fn [a line]
            (let [opp (subs line 0 1)
                  me (get-me-1 (subs line 2 3))]
              (+ a (get-result-score me opp) (get-choice-score me))))
          0
          (clojure.string/split-lines input)))

(defn get-me-2
  [needed-outcome opp]
  (condp = needed-outcome
    "X" (condp = opp
          "A" "C"
          "B" "A"
          "C" "B")
    "Y" (condp = opp
          "A" "A"
          "B" "B"
          "C" "C")
    "Z" (condp = opp
          "A" "B"
          "B" "C"
          "C" "A")))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input) 12))}
  [input]
  (reduce (fn [a line]
            (let [needed-outcome (subs line 2 3)
                  opp (subs line 0 1)
                  me (get-me-2 needed-outcome opp)]
              (+ a (get-result-score me opp) (get-choice-score me))))
          0
          (clojure.string/split-lines input)))

(comment
  (time (part-1 input))
  ; 10718
  ;; "Elapsed time: 4.870834 msecs"

  (time (part-2 input))
  ; 14652
  ;; "Elapsed time: 2.053541 msecs"
  )
