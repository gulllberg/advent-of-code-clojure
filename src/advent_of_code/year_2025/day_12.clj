(ns advent-of-code.year-2025.day-12)

;; This assumes perfect packing is possible, which is not true but gives the right answer (not for the test input).
;; (The shapes pack quite well, and as the number of packages increases, the amount of spill should approach 0.)

(def input (slurp "src/advent_of_code/year_2025/inputs/day12.txt"))

(defn parse-shape
  [shape-string]
  (count (re-seq #"#" shape-string)))

(defn parse-tree
  [tree-string]
  (let [[length width & amounts] (map read-string (re-seq #"\d+" tree-string))]
    {:width width
     :length length
     :amounts amounts}))

(defn parse-input
  [input]
  (let [parts (clojure.string/split input #"\n\n")
        shape-strings (drop-last parts)
        tree-strings (clojure.string/split-lines (last parts))]
    {:trees (map parse-tree tree-strings)
     :shapes (map parse-shape shape-strings)}))

(defn part-1
  [input]
  (let [{trees :trees shapes :shapes} (parse-input input)]
    (reduce (fn [a {width :width length :length amounts :amounts}]
              (let [tree-area (* width length)
                    min-area-needed (reduce (fn [area index]
                                              (+ area (* (nth amounts index) (nth shapes index))))
                                            0
                                            (range (count amounts)))]
                (if (> min-area-needed tree-area)
                  a
                  (inc a))))
            0
            trees)))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 11.702333 msecs"
  ;=> 476
  )
