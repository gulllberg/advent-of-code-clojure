(ns advent-of-code.year-2021.day-06)

(def input (slurp "src/advent_of_code/year_2021/inputs/day06.txt"))

(defn one-day
  [fish-timers]
  (reduce (fn [new-timers timer]
            (if (= timer 0)
              (conj new-timers 6 8)
              (conj new-timers (dec timer))))
          []
          fish-timers))

(defn n-days
  [fish-timers n]
  (reduce (fn [timers _]
            (one-day timers))
          fish-timers
          (range n)))

(defn part-1
  []
  (let [starting-timers (map read-string (clojure.string/split input #","))]
    (count (n-days starting-timers 80))))

(comment
  (time (part-1))
  ; 386536
  ; "Elapsed time: 128.647667 msecs"
  )

(defn one-day-dp
  [next-day]
  ;; Shifts everything one step and replaces first fish with sum of two "new" fish.
  (conj (drop-last 1 next-day) (+ (nth next-day 6) (nth next-day 8))))

(defn n-days-dp
  [n]
  (reduce (fn [total-created-fish _]
            (one-day-dp total-created-fish))
          [1 1 1 1 1 1 1 1 1]
          (range n)))

(defn part-2
  []
  (let [starting-timers (map read-string (clojure.string/split input #","))
        dp-list (n-days-dp 256)]
    (reduce (fn [sum timer]
              (+ sum (nth dp-list timer)))
            0
            starting-timers)))

(comment
  (count (n-days [1] 256))
  (time (part-2))
  ; 1732821262171
  ; "Elapsed time: 1.132042 msecs"
  )
