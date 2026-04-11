(ns advent-of-code.year-2019.day-04
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2019/inputs/day04.txt"))

(defn number->seq
  [number]
  (->> (str number)
       (re-seq #"\d")
       (map read-string)))

(defn has-adjacent-double?
  [number-seq]
  (->> (partition-by identity number-seq)
       (map count)
       (some (fn [c] (>= c 2)))))

(defn not-decreasing?
  [number-seq]
  (apply <= number-seq))

(defn valid-password?
  {:test (fn []
           (is (valid-password? 111111))
           (is-not (valid-password? 223450))
           (is-not (valid-password? 123789))
           (is-not (valid-password? 11111)))}
  [password]
  (let [number-seq (number->seq password)]
    (and (= 6 (count number-seq))
         (has-adjacent-double? number-seq)
         (not-decreasing? number-seq))))

(defn part-1
  [input]
  (let [[start end] (map read-string (re-seq #"\d+" input))]
    (->> (range start (inc end))
         (filter valid-password?)
         (count))))

(defn has-adjacent-double-2?
  {:test (fn []
           (is (has-adjacent-double-2? (number->seq 111122)))
           (is-not (has-adjacent-double-2? (number->seq 123444))))}
  [number-seq]
  (->> (partition-by identity number-seq)
       (map count)
       (some (fn [c] (= c 2)))))

(defn valid-password-2?
  {:test (fn []
           (is (valid-password-2? 112233))
           (is (valid-password-2? 111122))
           (is-not (valid-password-2? 123444)))}
  [password]
  (let [number-seq (number->seq password)]
    (and (= 6 (count number-seq))
         (has-adjacent-double-2? number-seq)
         (not-decreasing? number-seq))))

(defn part-2
  [input]
  (let [[start end] (map read-string (re-seq #"\d+" input))]
    (->> (range start (inc end))
         (filter valid-password-2?)
         (count))))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 1719.675666 msecs"
  ;=> 1099

  (time (part-2 input))
  ;; "Elapsed time: 1755.277042 msecs"
  ;=> 710
  )
