(ns advent-of-code.year-2017.day-11
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2017/inputs/day11.txt"))

(defn calculate-distance
  [x y]
  (let [x (max x (- x))
        y (max y (- y))]
    (if (>= x (* 2 y))
      x
      (int (+ x (- y (/ x 2)))))))

(defn part-1
  {:test (fn []
           (is= (part-1 "ne,ne,ne") 3)
           (is= (part-1 "ne,ne,sw,sw") 0)
           (is= (part-1 "se,sw,se,sw,sw") 3))}
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
    (calculate-distance x y)))

(defn part-2
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
                     [x y (max max-distance (calculate-distance x y))]))
                 [0 0 0])
         (last))))

(comment
  (time (part-1 input))
  ;; 747
  ;; "Elapsed time: 7.102166 msecs"

  (time (part-2 input))
  ;; 1544
  ;; "Elapsed time: 23.002875 msecs"
  )