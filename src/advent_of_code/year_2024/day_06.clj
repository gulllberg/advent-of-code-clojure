(ns advent-of-code.year-2024.day-06
  (:require [ysera.test :refer [is= is is-not]]
            [advent-of-code.year-2024.day-04 :refer [vector-add]]))

(def input (slurp "src/advent_of_code/year_2024/inputs/day06.txt"))
(def test-input "....#.....\n.........#\n..........\n..#.......\n.......#..\n..........\n.#..^.....\n........#.\n#.........\n......#...")

(defn parse-input
  [input]
  (let [lines (clojure.string/split-lines input)
        [room-map start-pos] (reduce (fn [[a start-pos] i]
                                       (let [line (nth lines i)]
                                         (reduce (fn [[a start-pos] j]
                                                   (let [c (nth line j)]
                                                     (condp = c
                                                       \^ [a [i j]]
                                                       \# [(conj a [i j]) start-pos]
                                                       [a start-pos])))
                                                 [a start-pos]
                                                 (range (count line)))))
                                     [#{} nil]
                                     (range (count lines)))]
    [room-map start-pos (count lines) (count (first lines))]))

(defn outside-room?
  [[i j] num-rows num-cols]
  (or (< i 0)
      (< j 0)
      (>= i num-rows)
      (>= j num-cols)))

(defn rotate-90-degrees-right
  [dir]
  [(second dir) (* -1 (first dir))])

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 41))}
  [input]
  (let [[room-map start-pos num-rows num-cols] (parse-input input)]
    (loop [dir [-1 0]
           pos start-pos
           visited #{start-pos}]
      (let [next-pos (vector-add pos dir)]
        (cond
          (outside-room? next-pos num-rows num-cols)
          (count visited)

          (contains? room-map next-pos)
          (recur (rotate-90-degrees-right dir)
                 pos
                 visited)
          :else
          (recur dir
                 next-pos
                 (conj visited next-pos)))))))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input) 42))}
  [input]
  42)

(comment
  ;; "Elapsed time: 17.465458 msecs"
  ;=> 5318
  (time (part-1 input))

  ;;
  (time (part-2 input))
  )
