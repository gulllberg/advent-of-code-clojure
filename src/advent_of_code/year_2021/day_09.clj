(ns advent-of-code.year-2021.day-09
  (:require [ysera.test :refer [is= is is-not]]
            [clojure.math.combinatorics :as combo]))

(def input (slurp "src/advent_of_code/year_2021/inputs/day09.txt"))
(def test-input "2199943210\n3987894921\n9856789892\n8767896789\n9899965678")

(defn get-height
  {:test (fn []
           (is= (get-height [[0 1] [2 3]] [1 0]) 2)
           (is= (get-height [[0 1] [2 3]] [1 2]) 9)
           (is= (get-height [[0 1] [2 3]] [2 0]) 9)
           (is= (get-height [[0 1] [2 3]] [2 2]) 9))}
  [grid [i j]]
  ; Default to 9, because it is easy to exclude in part b
  (-> (nth grid i [9])
      (nth j 9)))

(defn low-point?
  [grid [i j]]
  (let [height (get-height grid [i j])
        up-height (get-height grid [(dec i) j])
        right-height (get-height grid [i (inc j)])
        down-height (get-height grid [(inc i) j])
        left-height (get-height grid [i (dec j)])]
    (and (< height up-height)
         (< height right-height)
         (< height down-height)
         (< height left-height))))

(defn solve-a
  []
  (let [grid (map (fn [line]
                    (map read-string (clojure.string/split line #"")))
                  (clojure.string/split-lines input))]
    (reduce (fn [sum [i j]]
              (if (low-point? grid [i j])
                (+ sum (get-height grid [i j]) 1)
                sum))
            0
            (combo/cartesian-product (range (count grid)) (range (count (first grid)))))))

(comment
  (solve-a)
  ; 607
  )

(defn get-basin
  {:test (fn []
           (is= (get-basin (map (fn [line]
                                  (map read-string (clojure.string/split line #"")))
                                (clojure.string/split-lines test-input))
                           [0 1])
                #{[0 0] [0 1] [1 0]})
           (is= (get-basin (map (fn [line]
                                  (map read-string (clojure.string/split line #"")))
                                (clojure.string/split-lines test-input))
                           [4 6])
                #{[2 7] [3 6] [3 7] [3 8] [4 5] [4 6] [4 7] [4 8] [4 9]}))}
  [grid [i j]]
  (loop [basin #{[i j]}
         basin-candidates #{[(dec i) j] [i (inc j)] [(inc i) j] [i (dec j)]}]
    (if (empty? basin-candidates)
      basin
      (let [[i j] (first basin-candidates)
            up [(dec i) j]
            right [i (inc j)]
            down [(inc i) j]
            left [i (dec j)]]
        (if (and (not= (get-height grid [i j]) 9)
                 (or (and (contains? basin up) (> (get-height grid [i j]) (get-height grid up)))
                     (and (contains? basin right) (> (get-height grid [i j]) (get-height grid right)))
                     (and (contains? basin down) (> (get-height grid [i j]) (get-height grid down)))
                     (and (contains? basin left) (> (get-height grid [i j]) (get-height grid left)))))
          (recur (conj basin [i j]) (clojure.set/difference (conj basin-candidates up right down left) (conj basin [i j])))
          (recur basin (disj basin-candidates [i j])))))))

(defn solve-b
  []
  (let [grid (map (fn [line]
                    (map read-string (clojure.string/split line #"")))
                  (clojure.string/split-lines input))]
    (->> (reduce (fn [basin-sizes [i j]]
                   (if (low-point? grid [i j])
                     (conj basin-sizes (count (get-basin grid [i j])))
                     basin-sizes))
                 []
                 (combo/cartesian-product (range (count grid)) (range (count (first grid)))))
         (sort)
         (take-last 3)
         (apply *))))

(comment
  (solve-b)
  ; 900864
  )
