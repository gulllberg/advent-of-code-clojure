(ns advent-of-code.year-2020.day-11)

(def input (slurp "src/advent_of_code/year_2020/inputs/day11.txt"))

(defn create-state
  [input]
  (let [lines (clojure.string/split-lines input)]
    (reduce (fn [state i]
              (reduce (fn [state j]
                        (assoc state [i j] (condp = (nth (nth lines i) j)
                                             \L false
                                             \. nil
                                             \# true)))
                      state
                      (range (count (nth lines i)))))
            {}
            (range (count lines)))))

(defn get-n-occupied-adjacent
  [state position]
  (let [directions [[1 0] [1 -1] [0 -1] [-1 -1] [-1 0] [-1 1] [0 1] [1 1]]]
    (reduce (fn [n direction]
              (if (get state (mapv + position direction))
                (inc n)
                n))
            0
            directions)))

(defn get-next-state
  [state]
  (reduce (fn [new-state position]
            (condp = (get state position)
              true (assoc new-state position (if (>= (get-n-occupied-adjacent state position) 4) false true))
              false (assoc new-state position (if (= (get-n-occupied-adjacent state position) 0) true false))
              new-state))
          state
          (keys state)))

(defn solve-a
  []
  (loop [state (create-state input)]
    (let [new-state (get-next-state state)]
      (if (= state new-state)
        (reduce (fn [n-occupied position]
                  (if (get new-state position)
                    (inc n-occupied)
                    n-occupied))
                0
                (keys new-state))
        (recur new-state)))))

(comment
  (solve-a)
  ; 2472
  )

(defn get-n-occupied-adjacent-b
  [state position]
  (let [directions [[1 0] [1 -1] [0 -1] [-1 -1] [-1 0] [-1 1] [0 1] [1 1]]]
    (reduce (fn [n direction]
              (loop [position-to-check (mapv + position direction)]
                (condp = (get state position-to-check "outside")
                  "outside" n
                  true (inc n)
                  false n
                  (recur (mapv + position-to-check direction)))))
            0
            directions)))

(defn get-next-state-b
  [state]
  (reduce (fn [new-state position]
            (condp = (get state position)
              true (assoc new-state position (if (>= (get-n-occupied-adjacent-b state position) 5) false true))
              false (assoc new-state position (if (= (get-n-occupied-adjacent-b state position) 0) true false))
              new-state))
          state
          (keys state)))

(defn solve-b
  []
  (loop [state (create-state input)]
    (let [new-state (get-next-state-b state)]
      (if (= state new-state)
        (reduce (fn [n-occupied position]
                  (if (get new-state position)
                    (inc n-occupied)
                    n-occupied))
                0
                (keys new-state))
        (recur new-state)))))


(comment
  (solve-b)
  ; 2197
  )
