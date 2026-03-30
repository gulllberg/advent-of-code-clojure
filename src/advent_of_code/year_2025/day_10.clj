(ns advent-of-code.year-2025.day-10
  (:require [ysera.test :refer [is= is is-not]]))

;; With Tomas

(def input (slurp "src/advent_of_code/year_2025/inputs/day10.txt"))
(def test-input "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}\n[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}\n[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}")

(defn parse-input
  {:test (fn []
           (is= (parse-input test-input)
                [{:indicators-goal [0 1 1 0] :buttons [[3] [1 3] [2] [2 3] [0 2] [0 1]] :joltage-levels [3 5 4 7]}
                 {:indicators-goal [0 0 0 1 0] :buttons [[0 2 3 4] [2 3] [0 4] [0 1 2] [1 2 3 4]] :joltage-levels [7 5 12 7 2]}
                 {:indicators-goal [0 1 1 1 0 1] :buttons [[0 1 2 3 4] [0 3 4] [0 1 2 4 5] [1 2]] :joltage-levels [10 11 11 5 10 5]}]))}
  [input]
  (->> (clojure.string/split-lines input)
       (map (fn [row]
              (let [indicators-string (second (re-find #"\[([.#]+)\]" row))
                    button-strings (re-seq #"\([0-9,]+\)" row)
                    joltage-levels-string (->> (re-find #"\{[\d,]+\}" row)
                                               (re-seq #"\d+"))]
                {:indicators-goal (mapv (fn [c] (if (= c \#) 1 0)) indicators-string)
                 :buttons         (->> button-strings
                                       (mapv (fn [bs] (->> (re-seq #"\d+" bs)
                                                           (mapv read-string)))))
                 :joltage-levels  (mapv read-string joltage-levels-string)})))))

(defn button-press-to-get-indicator-goal
  {:test (fn []
           (is= (button-press-to-get-indicator-goal {:indicators-goal [0 1 1 0] :buttons [[3] [1 3] [2] [2 3] [0 2] [0 1]] :joltage-levels [3 5 4 7]})
                [[[0 2] [0 1]]
                 [[1 3] [2 3]]
                 [[3] [2] [2 3] [0 2] [0 1]]
                 [[3] [1 3] [2]]])
           (is= (button-press-to-get-indicator-goal {:indicators-goal [0 0 0 1 0] :buttons [[0 2 3 4] [2 3] [0 4] [0 1 2] [1 2 3 4]] :joltage-levels [7 5 12 7 2]})
                [[[0 4] [0 1 2] [1 2 3 4]]
                 [[0 2 3 4] [2 3] [0 1 2] [1 2 3 4]]])
           (is= (button-press-to-get-indicator-goal {:indicators-goal [0 1 1 1 0 1] :buttons [[0 1 2 3 4] [0 3 4] [0 1 2 4 5] [1 2]] :joltage-levels [10 11 11 5 10 5]})
                [[[0 3 4] [0 1 2 4 5]]
                 [[0 1 2 3 4] [0 1 2 4 5] [1 2]]]))}
  [machine]
  (->> (:buttons machine)
       (reduce (fn [states button]
                 (->> states
                      (mapcat (fn [state]
                                [state
                                 [(->> button
                                       (reduce (fn [a i]
                                                 (update a i (fn [v] (if (zero? v) 1 0))))
                                               (first state)))
                                  (conj (second state) button)]]))))
               [[(vec (repeat (count (:indicators-goal machine)) 0)) []]])
       (filter (fn [state] (= (first state) (:indicators-goal machine))))
       (map second)))

(defn count-button-press-to-get-indicator-goal
  {:test (fn []
           (is= (count-button-press-to-get-indicator-goal {:indicators-goal [0 1 1 0] :buttons [[3] [1 3] [2] [2 3] [0 2] [0 1]] :joltage-levels [3 5 4 7]})
                2)
           (is= (count-button-press-to-get-indicator-goal {:indicators-goal [0 0 0 1 0] :buttons [[0 2 3 4] [2 3] [0 4] [0 1 2] [1 2 3 4]] :joltage-levels [7 5 12 7 2]})
                3)
           (is= (count-button-press-to-get-indicator-goal {:indicators-goal [0 1 1 1 0 1] :buttons [[0 1 2 3 4] [0 3 4] [0 1 2 4 5] [1 2]] :joltage-levels [10 11 11 5 10 5]})
                2))}
  [machine]
  (->> (button-press-to-get-indicator-goal machine)
       (map count)
       (apply min)))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 7))}
  [input]
  (->> (parse-input input)
       (map count-button-press-to-get-indicator-goal)
       (reduce +)))

(declare count-button-press-to-get-joltage-levels)
(defn count-button-press-to-get-joltage-levels-raw
  {:test (fn []
           (is= (count-button-press-to-get-joltage-levels [[3] [1 3] [2] [2 3] [0 2] [0 1]]
                                                          [3 5 4 7])
                10)
           (is= (count-button-press-to-get-joltage-levels [[0 2 3 4] [2 3] [0 4] [0 1 2] [1 2 3 4]]
                                                          [7 5 12 7 2])
                12)
           (is= (count-button-press-to-get-joltage-levels [[0 1 2 3 4] [0 3 4] [0 1 2 4 5] [1 2]]
                                                          [10 11 11 5 10 5])
                11))}
  [buttons goal]
  (cond (every? zero? goal) 0

        (some neg? goal) ##Inf

        :else
        (let [even-odd-lights (->> goal (mapv (fn [v] (rem v 2))))
              pressed-buttons (button-press-to-get-indicator-goal {:buttons         buttons
                                                                   :indicators-goal even-odd-lights})]
          (if (empty? pressed-buttons)
            ##Inf
            (->> pressed-buttons
                 (map (fn [pbs]
                        (let [pressed (count pbs)
                              new-goal (->> (flatten pbs)
                                            (frequencies)
                                            (reduce-kv (fn [goal index n] (update goal index - n)) goal))]
                          (+ pressed
                             (* 2
                                (count-button-press-to-get-joltage-levels buttons
                                                                          (mapv (fn [x] (/ x 2)) new-goal)))))))
                 (apply min))))))

(def count-button-press-to-get-joltage-levels (memoize count-button-press-to-get-joltage-levels-raw))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input) 33))}
  [input]
  (->> (parse-input input)
       (map (fn [machine] (count-button-press-to-get-joltage-levels (:buttons machine) (:joltage-levels machine))))
       (reduce +)))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 78.879417 msecs"
  ;=> 481

  (time (part-2 input))
  ;; "Elapsed time: 12178.403583 msecs"
  ;=> 20142
  )
