(ns advent-of-code.year-2021.day-11
  (:require [ysera.test :refer [is= is is-not]]
            [clojure.math.combinatorics :as combo]))

(def input (slurp "src/advent_of_code/year_2021/inputs/day11.txt"))
(def test-input "5483143223\n2745854711\n5264556173\n6141336146\n6357385478\n4167524645\n2176841721\n6882881134\n4846848554\n5283751526")

(defn create-state
  [input]
  (let [lines (clojure.string/split-lines input)]
    (reduce (fn [s i]
              (let [cs (clojure.string/split (nth lines i) #"")]
                (reduce (fn [s j]
                          (assoc s [i j] (read-string (nth cs j))))
                        s
                        (range (count cs)))))
            {}
            (range (count lines)))))

(defn increment-all
  [state]
  (reduce (fn [s k]
            (update s k inc))
          state
          (keys state)))

(defn increment-neighbours
  [state k]
  (reduce (fn [s direction]
            (let [coordinates (mapv + k direction)]
              (if (contains? s coordinates)
                (update s coordinates inc)
                s)))
          state
          (combo/cartesian-product [-1 0 1] [-1 0 1])))

(defn flashes
  [state previous-flashes]
  (reduce (fn [[s p-f] k]
            (if (and (< 9 (get s k)) (not (contains? previous-flashes k)))
              [(increment-neighbours s k) (conj p-f k)]
              [s p-f]))
          [state previous-flashes]
          (keys state)))

(defn all-flashes
  [state]
  (loop [s state
         previous-flashes #{}]
    (let [[s p-f] (flashes s previous-flashes)]
      (if (= p-f previous-flashes)
        [s p-f]
        (recur s p-f)))))

(defn reset-flashed
  [state flashed]
  (reduce (fn [s k]
            (assoc s k 0))
          state
          flashed))

(defn one-step
  [state]
  (let [[s flashed] (-> state
                        (increment-all)
                        (all-flashes))]
    [(reset-flashed s flashed) (count flashed)]))

(defn n-steps
  {:test (fn []
           (is= (second (n-steps (create-state test-input) 100)) 1656)
           (is= (second (n-steps (create-state test-input) 2)) 35)
           (is= (second (n-steps (create-state test-input) 1)) 0))}
  [state n]
  (reduce (fn [[state number-of-flashes] _]
            (let [[state num-flashes] (one-step state)]
              [state (+ number-of-flashes num-flashes)]))
          [state 0]
          (range n)))

(defn part-1
  []
  (second (n-steps (create-state input) 100)))

(comment
  (time (part-1))
  ; 1793
  ; "Elapsed time: 17.742583 msecs"
  )

(defn part-2
  []
  (loop [state (create-state input)
         n 0]
    (let [[state num-flashes] (one-step state)
          n (inc n)]
      (if (= num-flashes 100)
        n
        (recur state n)))))

(comment
  (time (part-2))
  ; 247
  ; "Elapsed time: 49.50575 msecs"
  )
