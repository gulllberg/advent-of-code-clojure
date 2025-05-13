(ns advent-of-code.year-2015.day-13
  (:require [ysera.test :refer [is= is is-not]]
            [clojure.math.combinatorics :as combo]))

(def input (slurp "src/advent_of_code/year_2015/inputs/day13.txt"))
(def test-input "Alice would gain 54 happiness units by sitting next to Bob.\nAlice would lose 79 happiness units by sitting next to Carol.\nAlice would lose 2 happiness units by sitting next to David.\nBob would gain 83 happiness units by sitting next to Alice.\nBob would lose 7 happiness units by sitting next to Carol.\nBob would lose 63 happiness units by sitting next to David.\nCarol would lose 62 happiness units by sitting next to Alice.\nCarol would gain 60 happiness units by sitting next to Bob.\nCarol would gain 55 happiness units by sitting next to David.\nDavid would gain 46 happiness units by sitting next to Alice.\nDavid would lose 7 happiness units by sitting next to Bob.\nDavid would gain 41 happiness units by sitting next to Carol.")

(defn parse-input
  [input]
  (reduce (fn [a line]
            (let [[_ p1 modifier n p2] (re-find #"(\w+) would (gain|lose) (\d+) happiness units by sitting next to (\w+)." line)
                  change (read-string (str (if (= modifier "lose") "-" "") n))]
              (assoc-in a [p1 p2] change)))
          {}
          (clojure.string/split-lines input)))

(defn get-happiness-of-seating
  {:test (fn []
           (is= (get-happiness-of-seating (parse-input test-input) ["Carol" "David" "Alice" "Bob"])
                330))}
  [preferences seating]
  (reduce (fn [a [p1 p2]]
            (+ a (get-in preferences [p1 p2]) (get-in preferences [p2 p1])))
          0
          (partition 2 1 (take 1 seating) seating)))

(defn get-optimal-happiness
  [preferences]
  (let [[first-seat-person & people] (keys preferences)
        all-seatings (map (fn [p] (conj p first-seat-person)) (combo/permutations people))]
    (reduce (fn [a seating]
              (max a (get-happiness-of-seating preferences seating)))
            ##-Inf
            all-seatings)))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 330))}
  [input]
  (get-optimal-happiness (parse-input input)))

(defn add-yourself
  [preferences]
  (-> (reduce (fn [preferences k]
                (assoc-in preferences [k "yourself"] 0))
              preferences
              (keys preferences))
      (assoc "yourself" (zipmap (keys preferences) (repeat 0)))))

(defn part-2
  [input]
  (-> (parse-input input)
      (add-yourself)
      (get-optimal-happiness)))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 43.436167 msecs"
  ;=> 664

  (time (part-2 input))
  ;; "Elapsed time: 276.063667 msecs"
  ;=> 640
  )
