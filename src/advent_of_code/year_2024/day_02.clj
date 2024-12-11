(ns advent-of-code.year-2024.day-02
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2024/inputs/day02.txt"))
(def test-input "7 6 4 2 1\n1 2 7 8 9\n9 7 6 2 1\n1 3 2 4 5\n8 6 4 4 1\n1 3 6 7 9")

(defn check-report
  {:test (fn []
           (is (check-report [7 6 4 2 1]))
           (is-not (check-report [1 2 7 8 9]))
           (is-not (check-report [9 7 6 2 1]))
           (is-not (check-report [1 3 2 4 5]))
           (is-not (check-report [8 6 4 4 1]))
           (is (check-report [1 3 6 7 9])))}
  [report]
  (let [increasing (< (first report) (second report))]
    (loop [i 0]
      (let [curr (nth report i)
            next (nth report (inc i) nil)]
        (cond
          (= i (dec (count report)))
          true

          (and increasing
               (not (< curr next (+ curr 4))))
          false

          (and (not increasing)
               (not (> curr next (- curr 4))))
          false

          :else
          (recur (inc i)))))))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 2))}
  [input]
  (let [reports (->> (clojure.string/split-lines input)
                     (map (fn [line]
                            (->> (re-seq #"\d+" line)
                                 (map read-string)))))]
    (->> reports
         (filter check-report)
         (count))))

(defn check-all-possible-reports
  {:test (fn []
           (is (check-all-possible-reports [7 6 4 2 1]))
           (is-not (check-all-possible-reports [1 2 7 8 9]))
           (is-not (check-all-possible-reports [9 7 6 2 1]))
           (is (check-all-possible-reports [1 3 2 4 5]))
           (is (check-all-possible-reports [8 6 4 4 1]))
           (is (check-all-possible-reports [1 3 6 7 9])))}
  [report]
  (if (check-report report)
    true
    (loop [start []
           skipped (first report)
           end (rest report)]
      (let [report (concat start end)]
        (cond
          (check-report report)
          true

          (empty? end)
          false

          :else
          (recur (conj start skipped)
                 (first end)
                 (rest end)))))))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input) 4))}
  [input]
  (let [reports (->> (clojure.string/split-lines input)
                     (map (fn [line]
                            (->> (re-seq #"\d+" line)
                                 (map read-string)))))]
    (->> reports
         (filter check-all-possible-reports)
         (count))))

(comment
  ;; "Elapsed time: 10.443333 msecs"
  ;=> 660
  (time (part-1 input))

  ;; "Elapsed time: 10.614542 msecs"
  ;=> 683
  (time (part-2 input))
  )
