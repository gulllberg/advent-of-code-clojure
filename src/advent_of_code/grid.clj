(ns advent-of-code.grid
  (:require [ysera.test :refer [is= is is-not]]))

(defn parse-grid
  ([input parse-fn]
   (let [lines (into [] (clojure.string/split-lines input))]
     (reduce-kv (fn [a i line]
                  (reduce-kv (fn [a j c]
                               (assoc a [i j] (parse-fn c)))
                             a
                             (into [] line)))
                {}
                lines)))
  ([input]
   (parse-grid input identity)))

(def directions-without-diagonals [[-1 0] [1 0] [0 -1] [0 1]])
(def directions-with-diagonals (for [x (range -1 2)
                                     y (range -1 2)
                                     :when (not= 0 x y)]
                                 [x y]))

(defn get-neighbours
  ([position directions]
   (map (fn [dir]
          (mapv + position dir))
        directions))
  ([position]
   (get-neighbours position directions-without-diagonals)))

(defn turn-left
  {:test (fn []
           (is= (turn-left [0 1]) [-1 0])
           (is= (turn-left [0 -1]) [1 0])
           (is= (turn-left [1 0]) [0 1])
           (is= (turn-left [-1 0]) [0 -1]))}
  [[i j]]
  [(- j) i])

(defn turn-right
  {:test (fn []
           (is= (turn-right [-1 0]) [0 1])
           (is= (turn-right [1 0]) [0 -1])
           (is= (turn-right [0 1]) [1 0])
           (is= (turn-right [0 -1]) [-1 0]))}
  [[i j]]
  [j (- i)])

(defn manhattan-distance
  {:test (fn []
           (is= (manhattan-distance [1 2] [3 4]) 4)
           (is= (manhattan-distance [3 4] [1 2]) 4))}
  [p1 p2]
  (+ (abs (- (first p1) (first p2)))
     (abs (- (second p1) (second p2)))))