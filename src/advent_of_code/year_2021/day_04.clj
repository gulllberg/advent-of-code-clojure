(ns advent-of-code.year-2021.day-04
  (:require [ysera.test :refer [is= is is-not]]))

(def input-numbers (slurp "src/advent_of_code/year_2021/inputs/day04numbers.txt"))
(def input-boards (slurp "src/advent_of_code/year_2021/inputs/day04boards.txt"))

(defn board-string->board
  [board-string]
  (let [rows (map (fn [row-string]
                    (map (fn [p]
                           (read-string (apply str p)))
                         (partition 2 3 row-string)))
                  (clojure.string/split-lines board-string))
        columns (reduce (fn [columns i]
                          (conj columns (map (fn [row]
                                               (nth row i))
                                             rows)))
                        []
                        (range 5))]
    (concat rows columns)))

(defn filter-number
  [board number]
  (map (fn [row-or-column]
         (remove (fn [n]
                   (= n number))
                 row-or-column))
       board))

;, https://stackoverflow.com/questions/10192602/return-first-item-in-a-map-list-sequence-that-satisfies-a-predicate
(defn find-first
  [pred coll]
  (first (filter pred coll)))

(defn check-victory
  [board]
  (some empty? board))

(defn score-board
  [board last-number]
  (->> board
       (flatten)
       (set)
       (apply +)
       (* last-number)))

(defn play-number
  [boards number]
  (map (fn [board]
         (filter-number board number))
       boards))

(defn play-bingo
  [boards numbers]
  (reduce (fn [boards number]
            (let [new-boards (play-number boards number)]
              (if-let [winning-board (find-first check-victory new-boards)]
                (reduced (score-board winning-board number))
                new-boards)))
          boards
          numbers))

(defn part-1
  []
  (let [numbers (map read-string (clojure.string/split input-numbers #","))
        boards (map board-string->board (clojure.string/split input-boards #"\n\n"))]
    (play-bingo boards numbers)))


(comment
  (time (part-1))
  ; 67716
  ; "Elapsed time: 9.08275 msecs"
  )

(defn play-losing-bingo
  [boards numbers]
  (reduce (fn [boards number]
            (let [new-boards (play-number boards number)
                  losing-boards (remove check-victory new-boards)]
              (if (= (count losing-boards) 0)
                (reduced (score-board (first new-boards) number))
                losing-boards)))
          boards
          numbers))

(defn part-2
  []
  (let [numbers (map read-string (clojure.string/split input-numbers #","))
        boards (map board-string->board (clojure.string/split input-boards #"\n\n"))]
    (play-losing-bingo boards numbers)))


(comment
  (time (part-2))
  ; 1830
  ; "Elapsed time: 17.636375 msecs"
  )
