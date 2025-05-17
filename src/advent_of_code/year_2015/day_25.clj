(ns advent-of-code.year-2015.day-25
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2015/inputs/day25.txt"))

;; https://en.wikipedia.org/wiki/Pairing_function#Cantor_pairing_function
(defn cantor-pairing-function
  {:test (fn []
           (is= (cantor-pairing-function 0 0) 0)
           (is= (cantor-pairing-function 1 0) 1)
           (is= (cantor-pairing-function 0 1) 2)
           (is= (cantor-pairing-function 1 1) 4)
           (is= (cantor-pairing-function 2 2) 12)
           (is= (cantor-pairing-function 3 1) 11)
           (is= (cantor-pairing-function 2 3) 18))}
  [x y]
  (/ (+ (* x x) x (* 2 x y) (* 3 y) (* y y)) 2))

(defn get-order-in-sequence
  {:test (fn []
           (is= (get-order-in-sequence 2 2) 5)
           (is= (get-order-in-sequence 5 2) 17)
           (is= (get-order-in-sequence 2 4) 14))}
  [row column]
  (inc (cantor-pairing-function (dec row) (dec column))))

(defn get-next-code
  {:test (fn []
           (is= (get-next-code 20151125) 31916031))}
  [code]
  (-> code
      (* 252533)
      (rem 33554393)))

(defn get-code-n
  {:test (fn []
           (is= (get-code-n 6) 17289845))}
  [n]
  (nth (iterate get-next-code 20151125) (dec n)))

(defn part-1
  [input]
  (let [[row column] (map read-string (re-seq #"\d+" input))]
    (get-code-n (get-order-in-sequence row column))))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 226.513833 msecs"
  ;=> 2650453
  )