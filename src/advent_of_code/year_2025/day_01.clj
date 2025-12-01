(ns advent-of-code.year-2025.day-01
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2025/inputs/day01.txt"))
(def test-input "L68\nL30\nR48\nL5\nR60\nL55\nL1\nL99\nR14\nL82")

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 3))}
  [input]
  (->> (clojure.string/split-lines input)
       (reduce (fn [{dial :dial zeros :zeros} line]
                 (let [op (if (= \L (first line)) - +)
                       diff (read-string (re-find #"\d+" line))
                       dial (mod (op dial diff) 100)]
                   {:dial dial :zeros (if (zero? dial) (inc zeros) zeros)}))
               {:dial 50 :zeros 0})
       (:zeros)))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input) 6))}
  [input]
  (->> (clojure.string/split-lines input)
       (reduce (fn [{old-dial :dial zeros :zeros} line]
                 (let [op (if (= \L (first line)) - +)
                       diff (read-string (re-find #"\d+" line))
                       extra-rotations (quot diff 100)
                       diff (mod diff 100)
                       new-dial-before-mod (op old-dial diff)
                       new-dial (mod new-dial-before-mod 100)
                       has-zero-tick (or (zero? new-dial)
                                         (and (not (zero? old-dial))
                                              (or (neg? new-dial-before-mod)
                                                  (< 100 new-dial-before-mod))))]
                   {:dial new-dial :zeros (+ zeros extra-rotations (if has-zero-tick 1 0))}))
               {:dial 50 :zeros 0})
       (:zeros)))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 13.949375 msecs"
  ;=> 1071

  (time (part-2 input))
  ;; "Elapsed time: 10.457792 msecs"
  ;=> 6700
  )
