(ns advent-of-code.year-2017.day-06
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2017/inputs/day06.txt"))

(defn problem-6-reallocate
  {:test (fn []
           (is= (problem-6-reallocate [0 2 7 0]) [2 4 1 2]))}
  [memory-banks]
  (let [max-value (apply max memory-banks)
        max-index (.indexOf memory-banks max-value)]
    (-> (reduce (fn [[memory-banks index] _]
                  [(update memory-banks index inc)
                   (mod (inc index) (count memory-banks))])
                [(assoc memory-banks max-index 0) (mod (inc max-index) (count memory-banks))]
                (range max-value))
        (first))))

(defn problem-6a
  {:test (fn []
           (is= (problem-6a "0\t2\t7\t0") 5))}
  [input]
  (let [memory-banks (->> (clojure.string/split input #"\t")
                          (mapv read-string))]
    (loop [memory-banks memory-banks
           configurations #{}
           cycles 0]
      (if (contains? configurations memory-banks)
        cycles
        (recur (problem-6-reallocate memory-banks)
               (conj configurations memory-banks)
               (inc cycles))))))

(defn problem-6b
  {:test (fn []
           (is= (problem-6b "0\t2\t7\t0") 4))}
  [input]
  (let [memory-banks (->> (clojure.string/split input #"\t")
                          (mapv read-string))]
    (loop [memory-banks memory-banks
           configurations {}
           cycles 0]
      (if (contains? configurations memory-banks)
        (- cycles (get configurations memory-banks))
        (recur (problem-6-reallocate memory-banks)
               (assoc configurations memory-banks cycles)
               (inc cycles))))))

(comment
  (problem-6a input)
  ;; 4074
  (problem-6b input)
  ;; 2793
  )