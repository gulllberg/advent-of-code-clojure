(ns advent-of-code.year-2023.day-13
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2023/inputs/day13.txt"))
(def test-input "#.##..##.\n..#.##.#.\n##......#\n##......#\n..#.##.#.\n..##..##.\n#.#.##.#.\n\n#...##..#\n#....#..#\n..##..###\n#####.##.\n#####.##.\n..##..###\n#....#..#\n")

(defn find-reflection-point
  [lines]
  (loop [offset 1]
    (when (< offset (count lines))
      (let [first-half (reverse (take offset lines))
            second-half (drop offset lines)]
        (if (every? true? (map = first-half second-half))
          offset
          (recur (inc offset)))))))

(defn transpose-landscape
  {:test (fn []
           (is= (transpose-landscape [[1 2] [3 4]]) [[1 3] [2 4]]))}
  [rows]
  (reduce (fn [a [i row]]
            (reduce (fn [a [j c]]
                      (assoc-in a [j i] c))
                    a
                    (map-indexed (fn [j v]
                                   [j v])
                                 row)))
          (into [] (repeat (count (first rows)) []))
          (map-indexed (fn [i v]
                         [i v])
                       rows)))

(defn solve-landscape
  {:test (fn []
           (is= 4 (solve-landscape "#.#..#.##.#\n##....####.\n###..######\n.#....#..#.\n.##..##..##\n.##..##...#\n.######..##\n.#....#..#.\n##.##.####.")))}
  [landscape]
  (let [rows (clojure.string/split-lines landscape)]
    (if-let [row-reflection (find-reflection-point rows)]
      (* 100 row-reflection)
      (find-reflection-point (transpose-landscape rows)))))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 405))}
  [input]
  (let [landscapes (clojure.string/split input #"\n\n")]
    (reduce (fn [a v]
              (+ a (solve-landscape v)))
            0
            landscapes)))

(defn find-reflection-point-2
  [lines]
  (loop [offset 1]
    (when (< offset (count lines))
      (let [first-half (reverse (take offset lines))
            second-half (drop offset lines)]
        (if (= 1 (reduce + (map (fn [v1 v2]
                                  (count (remove true? (map = v1 v2))))
                                first-half second-half)))
          offset
          (recur (inc offset)))))))

(defn solve-landscape-2
  [landscape]
  (let [rows (clojure.string/split-lines landscape)]
    (if-let [row-reflection (find-reflection-point-2 rows)]
      (* 100 row-reflection)
      (find-reflection-point-2 (transpose-landscape rows)))))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input) 400))}
  [input]
  (let [landscapes (clojure.string/split input #"\n\n")]
    (reduce (fn [a v]
              (+ a (solve-landscape-2 v)))
            0
            landscapes)))

(comment
  (time (part-1 input))
  ;; 33735
  ;; "Elapsed time: 4.80943 msecs"

  (time (part-2 input))
  ;; 38063
  ;; "Elapsed time: 19.926282 msecs"
  )
