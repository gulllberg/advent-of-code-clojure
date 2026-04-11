(ns advent-of-code.year-2019.day-03
  (:require [ysera.test :refer [is= is is-not]]
            [advent-of-code.grid :refer [manhattan-distance]]))

(def input (slurp "src/advent_of_code/year_2019/inputs/day03.txt"))
(def test-input "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83")

(defn vector-multiply
  [u s]
  (map (fn [x] (* x s)) u))

(defn parse-line
  [line]
  (->> (clojure.string/split line #",")
       (reduce (fn [[points [position steps-taken]] move]
                 (let [direction (get {\R [1 0] \L [-1 0] \U [0 1] \D [0 -1]} (first move))
                       steps (read-string (re-find #"\d+" move))
                       new-positions-with-steps-taken (->> (range 1 (inc steps))
                                                           (map (fn [s]
                                                                  [(map + (vector-multiply direction s) position) (+ steps-taken s)])))]
                   [(reduce (fn [points [position steps-taken]]
                              (if (contains? points position)
                                points
                                (assoc points position steps-taken)))
                            points
                            new-positions-with-steps-taken)
                    (last new-positions-with-steps-taken)]))
               [{} [[0 0] 0]])
       (first)))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 159))}
  [input]
  (let [lines (clojure.string/split-lines input)
        first-wire-points (into #{} (keys (parse-line (first lines))))
        second-wire-points (into #{} (keys (parse-line (second lines))))]
    (->> (clojure.set/intersection first-wire-points second-wire-points)
         (map (fn [point]
                (manhattan-distance [0 0] point)))
         (apply min))))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input) 610))}
  [input]
  (let [lines (clojure.string/split-lines input)
        first-wire-points-with-steps (parse-line (first lines))
        second-wire-points-with-steps (parse-line (second lines))]
    (->> (clojure.set/intersection (into #{} (keys first-wire-points-with-steps))
                                   (into #{} (keys second-wire-points-with-steps)))
         (map (fn [point]
                (+ (get first-wire-points-with-steps point)
                   (get second-wire-points-with-steps point))))
         (apply min))))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 342.833583 msecs"
  ;=> 1983

  (time (part-2 input))
  ;; "Elapsed time: 438.94425 msecs"
  ;=> 107754
  )
