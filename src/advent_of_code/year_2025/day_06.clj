(ns advent-of-code.year-2025.day-06
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2025/inputs/day06.txt"))
(def test-input "123 328  51 64 \n 45 64  387 23 \n  6 98  215 314\n*   +   *   +  ")

(defn solve-math-problem
  {:test (fn []
           (is= (solve-math-problem [123 45 6 '*]) 33210)
           (is= (solve-math-problem [328 64 98 '+]) 490))}
  [problem]
  (apply (eval (last problem)) (drop-last problem)))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 4277556))}
  [input]
  (let [lines (clojure.string/split-lines input)]
    (->> lines
         (map (fn [line]
                (map read-string (re-seq #"\d+|\+|\*" line))))
         (apply interleave)
         (partition (count lines))
         (reduce (fn [a problem]
                   (+ a (solve-math-problem problem)))
                 0))))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input) 3263827))}
  [input]
  (let [lines (clojure.string/split-lines input)
        problem-seqs (map seq lines)
        operators (->> (last problem-seqs)
                       (filter (fn [c] (contains? #{\* \+} c)))
                       (map str)
                       (map read-string))
        numbers (->> (drop-last problem-seqs)
                     (apply interleave)
                     (partition (dec (count lines)))
                     (map (fn [cs]
                            (clojure.string/trim (apply str cs))))
                     (partition-by empty?)
                     (remove (fn [l]
                               (= l [""])))
                     (map (fn [l]
                            (map read-string l))))
        problems (map-indexed (fn [i ns]
                                (concat ns [(nth operators i)]))
                              numbers)]
    (reduce (fn [a problem]
              (+ a (solve-math-problem problem)))
            0
            problems)))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 9.260666 msecs"
  ;=> 5322004718681

  (time (part-2 input))
  ;; "Elapsed time: 31.849125 msecs"
  ;=> 9876636978528
  )
