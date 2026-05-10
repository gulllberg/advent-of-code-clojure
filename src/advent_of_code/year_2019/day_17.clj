(ns advent-of-code.year-2019.day-17
  (:require [ysera.test :refer [is= is is-not]]
            [advent-of-code.year-2019.intcode :refer [parse-program run-intcode-program]]
            [advent-of-code.grid :refer [parse-grid get-neighbours turn-left turn-right]]))

(def input (slurp "src/advent_of_code/year_2019/inputs/day17.txt"))

(defn get-output-string
  [output]
  (->> output
       (map char)
       (apply str)))

(defn print-output!
  [output]
  (-> (get-output-string output)
      (println)))

(defn find-intersections
  [grid]
  (->> (keys grid)
       (filter (fn [position]
                 (every? (fn [neighbour]
                           (contains? grid neighbour))
                         (get-neighbours position))))))

(defn get-alignment-parameter
  [[i j]]
  (* i j))

(defn part-1
  [input]
  (as-> (parse-program input) $
        (run-intcode-program $)
        (:program-output $)
        (get-output-string $)
        (parse-grid $ identity (fn [c] (not= c \.)))
        (find-intersections $)
        (map get-alignment-parameter $)
        (reduce + $)))

(defn get-next-instruction
  [grid position direction]
  (cond
    (contains? grid (mapv + position direction)) 1
    (contains? grid (mapv + position (turn-left direction))) "L"
    (contains? grid (mapv + position (turn-right direction))) "R"
    :else nil))

(defn get-starting-position
  [grid]
  (->> (keys grid)
       (filter (fn [position]
                 (not= (get grid position) \#)))
       (first)))

(defn get-starting-direction
  [grid]
  (let [starting-position (get-starting-position grid)]
    (condp = (get grid starting-position)
      \^ [-1 0]
      \v [1 0]
      \< [0 -1]
      \> [0 1])))

(defn traverse-grid
  [grid]
  (loop [position (get-starting-position grid)
         direction (get-starting-direction grid)
         instructions []]
    (let [next-instruction (get-next-instruction grid position direction)]
      (condp = next-instruction
        nil instructions
        1 (recur (mapv + position direction) direction (update instructions (dec (count instructions)) inc))
        "L" (recur (mapv + position (turn-left direction)) (turn-left direction) (conj instructions "L" 1))
        "R" (recur (mapv + position (turn-right direction)) (turn-right direction) (conj instructions "R" 1))))))

(defn get-path
  [input]
  (as-> (parse-program input) $
        (run-intcode-program $)
        (:program-output $)
        (get-output-string $)
        (parse-grid $ identity (fn [c] (not= c \.)))
        (traverse-grid $)
        (partition 2 $)))

(defn part-2
  [input]
  ;; Path found via get-path, and then manually translated into instructions as follows.
  (let [main "B,B,C,A,C,A,C,A,C,B\n"
        A "R,6,R,6,L,8,L,10\n"
        B "L,10,L,8,R,8,L,8,R,6\n"
        C "R,6,R,8,R,8\n"
        video "n\n"
        instructions-input (->> (str main A B C video)
                                (seq)
                                (map int))
        program (-> (parse-program input)
                    (assoc 0 2))]
    (-> (run-intcode-program program instructions-input)
        (:program-output)
        (last))))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 49.167375 msecs"
  ;=> 10064

  (time (part-2 input))
  ;; "Elapsed time: 89.0465 msecs"
  ;=> 1197725
  )
