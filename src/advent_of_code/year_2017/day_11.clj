(ns advent-of-code.year-2017.day-11
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2017/inputs/day11.txt"))

(defn problem-11-calculate-distance
  [x y]
  (let [x (max x (- x))
        y (max y (- y))]
    (if (>= x (* 2 y))
      x
      (int (+ x (- y (/ x 2)))))))

(defn problem-11a
  {:test (fn []
           (is= (problem-11a "ne,ne,ne") 3)
           (is= (problem-11a "ne,ne,sw,sw") 0)
           (is= (problem-11a "se,sw,se,sw,sw") 3))}
  [input]
  (let [steps (-> (clojure.string/trim input)
                  (clojure.string/split #","))
        [x y] (reduce (fn [[x y] step]
                        (condp = step
                          "n" [x (inc y)]
                          "ne" [(inc x) (+ y 0.5)]
                          "nw" [(dec x) (+ y 0.5)]
                          "s" [x (dec y)]
                          "se" [(inc x) (- y 0.5)]
                          "sw" [(dec x) (- y 0.5)]))
                      [0 0]
                      steps)]
    (problem-11-calculate-distance x y)))

(defn problem-11b
  [input]
  (let [steps (-> (clojure.string/trim input)
                  (clojure.string/split #","))]
    (->> steps
         (reduce (fn [[x y max-distance] step]
                   (let [[x y] (condp = step
                                 "n" [x (inc y)]
                                 "ne" [(inc x) (+ y 0.5)]
                                 "nw" [(dec x) (+ y 0.5)]
                                 "s" [x (dec y)]
                                 "se" [(inc x) (- y 0.5)]
                                 "sw" [(dec x) (- y 0.5)])]
                     [x y (max max-distance (problem-11-calculate-distance x y))]))
                 [0 0 0])
         (last))))

(comment
  (problem-11a input)
  ;; 747
  (problem-11b input)
  ;; 1544
  )