(ns advent-of-code.maths
  (:require [ysera.test :refer [is= is is-not]]))

(defn gcd
  {:test (fn []
           (is= (gcd 12 18) 6))}
  [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn lcm
  {:test (fn []
           (is= (lcm 4 6) 12))}
  [a b]
  (if (or (zero? a) (zero? b))
    0
    (/ (abs (* a b)) (gcd a b))))

(defn binomial
  [n k]
  ;; nCk = nC(n-k)
  (let [k (min k (- n k))]
    (if (zero? k)
      1
      (reduce *' 1 (map #(/ (- (+ n 1) %) %) (range 1 (inc k)))))))
