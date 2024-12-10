(ns advent-of-code.year-2020.day-09
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2020/inputs/day09.txt"))

(defn number-sum-of-other-numbers?
  {:test (fn []
           (is (number-sum-of-other-numbers? (range 26) 26))
           (is (number-sum-of-other-numbers? (range 26) 49))
           (is-not (number-sum-of-other-numbers? (range 26) 100))
           (is-not (number-sum-of-other-numbers? (range 26) 50)))}
  [numbers number]
  (loop [i 0
         j 1]
    (cond
      (= (+ (nth numbers i) (nth numbers j)) number) true
      (= i (- (count numbers) 2)) false                     ; If i second to last number j must be last number and this is final check
      (= j (dec (count numbers))) (recur (inc i) (+ i 2))
      :else (recur i (inc j)))))

(defn solve-a
  []
  (let [preamble-length 25
        numbers (vec (map read-string (clojure.string/split-lines input)))]
    (loop [i 0]
      (if (number-sum-of-other-numbers? (subvec numbers i (+ i preamble-length)) (nth numbers (+ i preamble-length)))
        (recur (inc i))
        (nth numbers (+ i preamble-length))))))

(comment
  (solve-a)
  ; 26796446
  )

(defn solve-b
  []
  (let [numbers (vec (map read-string (clojure.string/split-lines input)))
        target-number 26796446                              ;; From a
        ]
    (loop [length 2
           i 0]
      (let [number-range (subvec numbers i (+ i length))]
        (cond
          (= (apply + number-range) target-number) (+ (apply min number-range) (apply max number-range))
          (= (+ i length) (count numbers)) (recur (inc length) 0)
          :else (recur length (inc i)))))))

(comment
  (solve-b)
  ; 3353494
  )
