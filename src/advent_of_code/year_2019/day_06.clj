(ns advent-of-code.year-2019.day-06
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2019/inputs/day06.txt"))
(def test-input "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L")
(def test-input-2 "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN")

(defn parse-input
  [input]
  (->> (clojure.string/split-lines input)
       (reduce (fn [a line]
                 (let [[p1 p2] (clojure.string/split line #"\)")]
                   (assoc a p2 p1)))
               {})))

(declare count-orbits)
(defn count-orbits-raw
  [state planet end-planet]
  (if (= planet end-planet)
    0
    (let [orbits-around (get state planet)]
      (if orbits-around
        (+ 1 (count-orbits state orbits-around end-planet))
        ##Inf))))
(def count-orbits (memoize count-orbits-raw))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 42))}
  [input]
  (let [state (parse-input input)]
    (->> (keys state)
         (reduce (fn [a p]
                   (+ a (count-orbits state p "COM")))
                 0))))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input-2) 4))}
  [input]
  (let [state (parse-input input)]
    (->> (keys state)
         (reduce (fn [a p]
                   (min a (+ (count-orbits state "SAN" p)
                             (count-orbits state "YOU" p)
                             -2)))
                 ##Inf))))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 18.718292 msecs"
  ;=> 234446

  (time (part-2 input))
  ;; "Elapsed time: 577.4275 msecs"
  ;=> 385
  )
