(ns advent-of-code.year-2017.day-06
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2017/inputs/day06.txt"))

(defn reallocate
  {:test (fn []
           (is= (reallocate [0 2 7 0]) [2 4 1 2]))}
  [memory-banks]
  (let [max-value (apply max memory-banks)
        max-index (.indexOf memory-banks max-value)]
    (-> (reduce (fn [[memory-banks index] _]
                  [(update memory-banks index inc)
                   (mod (inc index) (count memory-banks))])
                [(assoc memory-banks max-index 0) (mod (inc max-index) (count memory-banks))]
                (range max-value))
        (first))))

(defn part-1
  {:test (fn []
           (is= (part-1 "0\t2\t7\t0") 5))}
  [input]
  (let [memory-banks (->> (clojure.string/split input #"\t")
                          (mapv read-string))]
    (loop [memory-banks memory-banks
           configurations #{}
           cycles 0]
      (if (contains? configurations memory-banks)
        cycles
        (recur (reallocate memory-banks)
               (conj configurations memory-banks)
               (inc cycles))))))

(defn part-2
  {:test (fn []
           (is= (part-2 "0\t2\t7\t0") 4))}
  [input]
  (let [memory-banks (->> (clojure.string/split input #"\t")
                          (mapv read-string))]
    (loop [memory-banks memory-banks
           configurations {}
           cycles 0]
      (if (contains? configurations memory-banks)
        (- cycles (get configurations memory-banks))
        (recur (reallocate memory-banks)
               (assoc configurations memory-banks cycles)
               (inc cycles))))))

(comment
  (time (part-1 input))
  ;; 4074
  ;; "Elapsed time: 33.683208 msecs"

  (time (part-2 input))
  ;; 2793
  ;; "Elapsed time: 33.927625 msecs"
  )