(ns advent-of-code.year-2015.day-25
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2015/inputs/day25.txt"))

(defn get-first-number-on-row
  {:test (fn []
           (is= (get-first-number-on-row 1) 1)
           (is= (get-first-number-on-row 3) 4)
           (is= (get-first-number-on-row 6) 16))}
  [row]
  (->> (range)
       (take row)
       (reduce +)
       (inc)))

(defn get-difference-between-column-1-and-n
  {:test (fn []
           (is= (get-difference-between-column-1-and-n 1 3) 5)
           (is= (get-difference-between-column-1-and-n 3 3) 9)
           (is= (get-difference-between-column-1-and-n 3 2) 4)
           (is= (get-difference-between-column-1-and-n 2 5) 18))}
  [row column]
  (->> (range (inc row) (+ row column))
       (reduce +)))

(defn get-order-in-sequence
  {:test (fn []
           (is= (get-order-in-sequence 2 2) 5)
           (is= (get-order-in-sequence 5 2) 17)
           (is= (get-order-in-sequence 2 4) 14))}
  [row column]
  (let [first-number-on-row (get-first-number-on-row row)
        difference-between-column-1-and-n (get-difference-between-column-1-and-n row column)]
    (+ first-number-on-row difference-between-column-1-and-n)))

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
  (loop [code 20151125
         n n]
    (if (= n 1)
      code
      (recur (get-next-code code) (dec n)))))

(defn part-1
  [input]
  (let [[row column] (map read-string (re-seq #"\d+" input))]
    (get-code-n (get-order-in-sequence row column))))


(comment
  (time (part-1 input))
  ;; "Elapsed time: 226.513833 msecs"
  ;=> 2650453
  )