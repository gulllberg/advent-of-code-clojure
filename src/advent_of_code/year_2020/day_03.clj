(ns advent-of-code.year-2020.day-03
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2020/inputs/day03.txt"))

(defn get-str-at-column
  {:test (fn []
           (is= (get-str-at-column "#..#" 2) ".")
           (is= (get-str-at-column "#..#" 4) "#"))}
  [row-string column-index]
  (let [n-columns (count row-string)]
    (str (nth row-string (mod column-index n-columns)))))

(defn check-slope
  {:test (fn []
           (is= (check-slope (clojure.string/split-lines input) 3 1) 148))}
  [map-rows right-n down-n]
  (reduce (fn [n-trees row-index]
            (let [column-index (* row-index (/ right-n down-n))]
              (if (= "#" (get-str-at-column (nth map-rows row-index) column-index))
                (inc n-trees)
                n-trees)))
          0
          (range 0 (count map-rows) down-n)))

(defn solve-a
  []
  (check-slope (clojure.string/split-lines input) 3 1))

(comment
  (solve-a)
  ; 148
  )

(defn solve-b
  []
  (let [map-rows (clojure.string/split-lines input)]
    (reduce (fn [a [right-n down-n]]
              (* a (check-slope map-rows right-n down-n)))
            1
            [[1 1] [3 1] [5 1] [7 1] [1 2]])))

(comment
  (solve-b)
  ; 727923200
  )
