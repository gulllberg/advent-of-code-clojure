(ns advent-of-code.year-2015.day-14
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2015/inputs/day14.txt"))
(def test-input "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.\nDancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.")

(defn parse-input
  [input]
  (reduce (fn [a line]
            (conj a (map read-string (re-seq #"\d+" line))))
          []
          (clojure.string/split-lines input)))

(defn get-reindeer-distance
  [[speed duration resting-time] t]
  (let [cycle-length (+ duration resting-time)
        cycle-distance (* speed duration)
        n-cycles (quot t cycle-length)
        remaining-time (rem t cycle-length)
        remaining-distance (* speed (min remaining-time duration))]
    (+ remaining-distance (* n-cycles cycle-distance))))

(defn get-winning-distance
  {:test (fn []
           (is= (get-winning-distance (parse-input test-input) 1000)
                1120))}
  [reindeers t]
  (->> reindeers
       (map (fn [reindeer]
              (get-reindeer-distance reindeer t)))
       (apply max)))

(defn part-1
  [input]
  (get-winning-distance (parse-input input) 2503))

(defn get-winning-score
  {:test (fn []
           (is= (get-winning-score (parse-input test-input) 1000)
                689))}
  [reindeers time]
  (loop [t 1
         scores (into [] (repeat (count reindeers) 0))]
    (if (> t time)
      (apply max scores)
      (let [distances (map (fn [reindeer]
                             (get-reindeer-distance reindeer t))
                           reindeers)
            leading-distance (apply max distances)]
        (recur (inc t)
               (reduce (fn [scores i]
                            (if (= (nth distances i) leading-distance)
                              (update scores i inc)
                              scores))
                          scores
                          (range (count scores))))))))

(defn part-2
  [input]
  (get-winning-score (parse-input input) 2503))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 0.355 msecs"
  ;=> 2660

  (time (part-2 input))
  ;; "Elapsed time: 18.011292 msecs"
  ;=> 1256
  )
