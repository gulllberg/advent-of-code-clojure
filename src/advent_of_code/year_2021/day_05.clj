(ns advent-of-code.year-2021.day-05
  (:require [ysera.test :refer [is= is is-not]]
            [clojure.math.combinatorics :as combo]))

(def input (slurp "src/advent_of_code/year_2021/inputs/day05.txt"))

(defn row->coordinates
  {:test (fn []
           (is= (row->coordinates "0,9 -> 5,9") [[0 9] [5 9]])
           (is= (row->coordinates "500,965 -> 531,956") [[500 965] [531 956]]))}
  [row]
  (->> row
       (re-seq #"\d+")
       (map read-string)
       (partition 2)))

(defn horizontal-or-vertical?
  {:test (fn []
           (is (horizontal-or-vertical? [[252 460] [252 461]]))
           (is-not (horizontal-or-vertical? [[912 930] [64 82]])))}
  [line]
  (let [x1 (first (first line))
        x2 (first (second line))
        y1 (second (first line))
        y2 (second (second line))]
    (or (= x1 x2)
        (= y1 y2))))

(defn mark-points-for-line
  {:test (fn []
           (is= (mark-points-for-line {} [[252 460] [252 461]]) {[252 460] 1 [252 461] 1})
           (is= (mark-points-for-line {[1 1] 1} [[1 1] [3 3]]) {[1 1] 2 [2 2] 1 [3 3] 1})
           (is= (mark-points-for-line {} [[1 3] [3 1]]) {[1 3] 1 [2 2] 1 [3 1] 1}))}
  [points line]
  (let [x1 (first (first line))
        x2 (first (second line))
        y1 (second (first line))
        y2 (second (second line))]
    (reduce (fn [points coordinate]
              (update points coordinate (fn [c]
                                          (inc (or c 0)))))
            points
            (if (horizontal-or-vertical? line)
              (combo/cartesian-product (range (min x1 x2) (inc (max x1 x2))) (range (min y1 y2) (inc (max y1 y2))))
              ;; Should be able to get the diagonal range in a prettier way...
              (map (fn [i]
                     [(+ x1 (* i (if (< x1 x2) 1 -1))) (+ y1 (* i (if (< y1 y2) 1 -1)))])
                   (range (inc (- (max x1 x2) (min x1 x2)))))))))

(defn mark-points
  [lines]
  (reduce mark-points-for-line {} lines))

(defn part-1
  []
  (->> (clojure.string/split-lines input)
       (map row->coordinates)
       (filter horizontal-or-vertical?)
       (mark-points)
       (vals)
       (filter (fn [v] (<= 2 v)))
       (count)))

(comment
  (time (part-1))
  ; 6572
  ; "Elapsed time: 88.403417 msecs"
  )

(defn part-2
  []
  (->> (clojure.string/split-lines input)
       (map row->coordinates)
       (mark-points)
       (vals)
       (filter (fn [v] (<= 2 v)))
       (count)))


(comment
  (time (part-2))
  ; 21466
  ; "Elapsed time: 141.059084 msecs"
  )
