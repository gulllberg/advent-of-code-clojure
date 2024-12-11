(ns advent-of-code.year-2020.day-17
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2020/inputs/day17.txt"))

(defn get-neighbours
  [coordinate]
  (let [directions [[-1 -1 -1]
                    [-1 -1 0]
                    [-1 -1 1]
                    [-1 0 -1]
                    [-1 0 0]
                    [-1 0 1]
                    [-1 1 -1]
                    [-1 1 0]
                    [-1 1 1]
                    [0 -1 -1]
                    [0 -1 0]
                    [0 -1 1]
                    [0 0 -1]
                    [0 0 1]
                    [0 1 -1]
                    [0 1 0]
                    [0 1 1]
                    [1 -1 -1]
                    [1 -1 0]
                    [1 -1 1]
                    [1 0 -1]
                    [1 0 0]
                    [1 0 1]
                    [1 1 -1]
                    [1 1 0]
                    [1 1 1]]]
    (map (fn [d]
           (mapv + coordinate d))
         directions)))

(defn create-state
  [input]
  (let [rows (clojure.string/split-lines input)]
    (reduce (fn [state row-i]
              (let [row (nth rows row-i)]
                (reduce (fn [state column-i]
                          (if (= \# (nth row column-i))
                            (-> state
                                (assoc [row-i column-i 0] true)
                                (update :candidates clojure.set/union (into #{} (get-neighbours [row-i column-i 0])) #{[row-i column-i 0]}))
                            state))
                        state
                        (range (count row)))))
            {:candidates #{}}
            (range (count rows)))))

(defn get-number-of-active-neighbours
  [state coordinate]
  (reduce (fn [n neighbour]
            (if (get state neighbour)
              (inc n)
              n))
          0
          (get-neighbours coordinate)))

(defn get-next-state
  [state]
  (reduce (fn [new-state candidate]
            (let [candidate-active (get state candidate)
                  number-of-active-neighbours (get-number-of-active-neighbours state candidate)]
              (if candidate-active
                (if (or (= number-of-active-neighbours 2) (= number-of-active-neighbours 3))
                  new-state
                  (-> new-state
                      (assoc candidate false)
                      (update :candidates clojure.set/union (into #{} (get-neighbours candidate)))))
                (if (= number-of-active-neighbours 3)
                  (-> new-state
                      (assoc candidate true)
                      (update :candidates clojure.set/union (into #{} (get-neighbours candidate))))
                  new-state))))
          (assoc state :candidates #{})
          (:candidates state)))

(defn do-n-cycles
  [state n]
  (loop [state state
         i 0]
    (if (= i n)
      state
      (recur (get-next-state state) (inc i)))))

(defn get-number-of-active
  {:test (fn []
           (is= (get-number-of-active (do-n-cycles (create-state ".#.\n..#\n###") 6)) 112))}
  [state]
  (as-> state $
        (dissoc $ :candidates)
        (vals $)
        (filter true? $)
        (count $)))

(defn part-1
  []
  (get-number-of-active (do-n-cycles (create-state input) 6)))

(comment
  (time (part-1))
  ; 346
  ; "Elapsed time: 98.922625 msecs"
  )

(defn get-neighbours-2
  [coordinate]
  (let [directions [[-1 -1 -1 -1]
                    [-1 -1 -1 0]
                    [-1 -1 -1 1]
                    [-1 -1 0 -1]
                    [-1 -1 0 0]
                    [-1 -1 0 1]
                    [-1 -1 1 -1]
                    [-1 -1 1 0]
                    [-1 -1 1 1]
                    [-1 0 -1 -1]
                    [-1 0 -1 0]
                    [-1 0 -1 1]
                    [-1 0 0 -1]
                    [-1 0 0 0]
                    [-1 0 0 1]
                    [-1 0 1 -1]
                    [-1 0 1 0]
                    [-1 0 1 1]
                    [-1 1 -1 -1]
                    [-1 1 -1 0]
                    [-1 1 -1 1]
                    [-1 1 0 -1]
                    [-1 1 0 0]
                    [-1 1 0 1]
                    [-1 1 1 -1]
                    [-1 1 1 0]
                    [-1 1 1 1]
                    [0 -1 -1 -1]
                    [0 -1 -1 0]
                    [0 -1 -1 1]
                    [0 -1 0 -1]
                    [0 -1 0 0]
                    [0 -1 0 1]
                    [0 -1 1 -1]
                    [0 -1 1 0]
                    [0 -1 1 1]
                    [0 0 -1 -1]
                    [0 0 -1 0]
                    [0 0 -1 1]
                    [0 0 0 -1]
                    [0 0 0 1]
                    [0 0 1 -1]
                    [0 0 1 0]
                    [0 0 1 1]
                    [0 1 -1 -1]
                    [0 1 -1 0]
                    [0 1 -1 1]
                    [0 1 0 -1]
                    [0 1 0 0]
                    [0 1 0 1]
                    [0 1 1 -1]
                    [0 1 1 0]
                    [0 1 1 1]
                    [1 -1 -1 -1]
                    [1 -1 -1 0]
                    [1 -1 -1 1]
                    [1 -1 0 -1]
                    [1 -1 0 0]
                    [1 -1 0 1]
                    [1 -1 1 -1]
                    [1 -1 1 0]
                    [1 -1 1 1]
                    [1 0 -1 -1]
                    [1 0 -1 0]
                    [1 0 -1 1]
                    [1 0 0 -1]
                    [1 0 0 0]
                    [1 0 0 1]
                    [1 0 1 -1]
                    [1 0 1 0]
                    [1 0 1 1]
                    [1 1 -1 -1]
                    [1 1 -1 0]
                    [1 1 -1 1]
                    [1 1 0 -1]
                    [1 1 0 0]
                    [1 1 0 1]
                    [1 1 1 -1]
                    [1 1 1 0]
                    [1 1 1 1]]]
    (map (fn [d]
           (mapv + coordinate d))
         directions)))

(defn create-state-2
  [input]
  (let [rows (clojure.string/split-lines input)]
    (reduce (fn [state row-i]
              (let [row (nth rows row-i)]
                (reduce (fn [state column-i]
                          (if (= \# (nth row column-i))
                            (-> state
                                (assoc [row-i column-i 0 0] true)
                                (update :candidates clojure.set/union (into #{} (get-neighbours-2 [row-i column-i 0 0])) #{[row-i column-i 0 0]}))
                            state))
                        state
                        (range (count row)))))
            {:candidates #{}}
            (range (count rows)))))

(defn get-number-of-active-neighbours-2
  [state coordinate]
  (reduce (fn [n neighbour]
            (if (get state neighbour)
              (inc n)
              n))
          0
          (get-neighbours-2 coordinate)))

(defn get-next-state-2
  [state]
  (reduce (fn [new-state candidate]
            (let [candidate-active (get state candidate)
                  number-of-active-neighbours (get-number-of-active-neighbours-2 state candidate)]
              (if candidate-active
                (if (or (= number-of-active-neighbours 2) (= number-of-active-neighbours 3))
                  new-state
                  (-> new-state
                      (assoc candidate false)
                      (update :candidates clojure.set/union (into #{} (get-neighbours-2 candidate)))))
                (if (= number-of-active-neighbours 3)
                  (-> new-state
                      (assoc candidate true)
                      (update :candidates clojure.set/union (into #{} (get-neighbours-2 candidate))))
                  new-state))))
          (assoc state :candidates #{})
          (:candidates state)))

(defn do-n-cycles-2
  [state n]
  (loop [state state
         i 0]
    (if (= i n)
      state
      (recur (get-next-state-2 state) (inc i)))))

(defn get-number-of-active-2
  {:test (fn []
           (is= (get-number-of-active-2 (do-n-cycles-2 (create-state-2 ".#.\n..#\n###") 6)) 848))}
  [state]
  (as-> state $
        (dissoc $ :candidates)
        (vals $)
        (filter true? $)
        (count $)))

(defn part-2
  []
  (get-number-of-active-2 (do-n-cycles-2 (create-state-2 input) 6)))

(comment
  (time (part-2))
  ; 1632
  ; "Elapsed time: 3589.517958 msecs"
  )
