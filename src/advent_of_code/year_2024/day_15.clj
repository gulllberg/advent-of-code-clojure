(ns advent-of-code.year-2024.day-15
  (:require [ysera.test :refer [is= is is-not]]))

; with Mattias and Tomas

(def input (-> (slurp "src/advent_of_code/year_2024/inputs/day15.txt")
               (clojure.string/split #"\n\n")))
(def test-input (-> (slurp "src/advent_of_code/year_2024/day15-test-input.txt")
                    (clojure.string/split #"\n\n")))

(defn parse-input
  [input]
  (apply merge (for [[y row] (map-indexed vector input)
                     [x p] (map-indexed vector row)]
                 {[x y] p})))

(defn all-of-type
  [ch the-map]
  (into #{} (keys (filter (fn [[_ v]] (= v ch)) the-map))))

(defn create-warehouse
  [[input moves]]
  (let [warehouse (parse-input (clojure.string/split input #"\n"))]
    {:robot (first (all-of-type \@ warehouse))
     :walls (all-of-type \# warehouse)
     :boxes (all-of-type \O warehouse)
     :moves (clojure.string/replace moves #"\n" "")}))

(def parse-move {\^ [0 -1] \> [1 0] \v [0 1] \< [-1 0]})
(def warehouse (create-warehouse input))
(def test-warehouse (create-warehouse test-input))

(defn get-movable-boxes
  {:test (fn []
           (is= (get-movable-boxes test-warehouse [-1 0] [4 4])
                {[3 4] [2 4]})
           (is= (get-movable-boxes test-warehouse [-1 0] [1 1])
                :wall))}
  [warehouse dir pos]
  (loop [movable-boxes {}
         pos pos]
    (let [new-pos (mapv + dir pos)]
      (cond (get-in warehouse [:walls new-pos]) :wall
            (get-in warehouse [:boxes new-pos]) (recur (assoc movable-boxes new-pos (mapv + dir new-pos)) new-pos)
            :else movable-boxes))))

(defn move [warehouse]
  (->> (:moves warehouse)
       (reduce (fn [warehouse move]
                 (let [dir (parse-move move)
                       pos (warehouse :robot)
                       movable-boxes (get-movable-boxes warehouse dir pos)]
                   (if (= movable-boxes :wall)
                     warehouse
                     (-> warehouse
                         (assoc :robot (mapv + dir pos))
                         (update :boxes (fn [boxes] (apply disj boxes (keys movable-boxes))))
                         (update :boxes (fn [boxes] (apply conj boxes (vals movable-boxes))))))))
               warehouse)))

(defn part-1
  {:test (fn []
           (is= (part-1 test-warehouse) 10092))}
  [warehouse]
  (->> warehouse
       (move)
       (:boxes)
       (map (fn [[x y]] (+ x (* 100 y))))
       (reduce +)))

(defn parse-atlas-input
  [input]
  (apply merge (for [[y row] (map-indexed vector input)
                     [x p] (map-indexed vector row)]
                 {[x y] p})))

(def test-atlas-input (parse-atlas-input (clojure.string/split-lines (first test-input))))
(def atlas-input (parse-atlas-input (clojure.string/split-lines (first input))))

(defn widen [[x y]] [(* 2 x) y])

(defn create-warehouse-2
  [atlas-input moves]
  {:robot (widen (first (all-of-type \@ atlas-input)))
   :walls (->> (all-of-type \# atlas-input)
               (map (fn [p] (let [wp (widen p)] [wp (update wp 0 inc)])))
               (apply concat)
               (into #{}))
   :boxes (->> (all-of-type \O atlas-input)
               (map (fn [p] (let [wp (widen p)] [wp (update wp 0 inc)])))
               (into #{}))
   :moves moves})

(def moves (->> (clojure.string/replace (second input) #"\n" "")
                (map parse-move)))
(def test-moves (->> (clojure.string/replace (second test-input) #"\n" "")
                     (map parse-move)))

(def warehouse-2 (create-warehouse-2 atlas-input moves))
(def test-warehouse-2 (create-warehouse-2 test-atlas-input test-moves))

(defn wall?
  {:test (fn []
           (is (wall? test-warehouse-2 [0 1]))
           (is (wall? test-warehouse-2 [1 1]))
           (is-not (wall? test-warehouse-2 [2 1])))}
  [warehouse position]
  (get-in warehouse [:walls position]))

(defn get-box
  {:test (fn []
           (is= (get-box test-warehouse-2 [2 5]) [[2 5] [3 5]])
           (is= (get-box test-warehouse-2 [3 5]) [[2 5] [3 5]])
           (is-not (get-box test-warehouse-2 [4 5])))}
  [warehouse position]
  (or (get-in warehouse [:boxes [position (update position 0 inc)]])
      (get-in warehouse [:boxes [(update position 0 dec) position]])))

(defn move-box
  {:test (fn []
           (is= (move-box [[6 4] [7 4]] [-1 0]) [[5 4] [6 4]])
           (is= (move-box [[6 4] [7 4]] [0 -1]) [[6 3] [7 3]]))}
  [box dir]
  (->> box
       (mapv (fn [p] (mapv + p dir)))))

(defn get-movable-boxes-2
  {:test (fn []
           (is= (get-movable-boxes-2 test-warehouse-2 [-1 0] [8 4])
                {[[6 4] [7 4]] [[5 4] [6 4]]})
           (is= (get-movable-boxes-2 test-warehouse-2 [0 -1] [4 4])
                {[[4 3] [5 3]] [[4 2] [5 2]]})
           (is= (get-movable-boxes-2 test-warehouse-2 [0 -1] [5 4])
                {[[4 3] [5 3]] [[4 2] [5 2]]})
           (is= (get-movable-boxes-2 {:boxes #{[[4 4] [5 4]] [[6 4] [7 4]] [[5 5] [6 5]]}} [0 -1] [5 6])
                {[[4 4] [5 4]] [[4 3] [5 3]] [[6 4] [7 4]] [[6 3] [7 3]] [[5 5] [6 5]] [[5 4] [6 4]]})
           (is= (get-movable-boxes-2 test-warehouse-2 [-1 0] [2 1])
                :wall))}
  [warehouse dir pos]
  (let [x-move (zero? (second dir))]
    (loop [moveable-boxes {}
           pos pos]
      (let [new-pos (mapv + dir pos)
            box (get-box warehouse new-pos)]
        (cond (wall? warehouse new-pos) :wall

              (and box x-move)
              (recur (assoc moveable-boxes box (move-box box dir)) (mapv + dir new-pos))

              box
              (let [left-boxes (get-movable-boxes-2 warehouse dir (first box))
                    right-boxes (get-movable-boxes-2 warehouse dir (second box))]
                (if (or (= :wall left-boxes) (= :wall right-boxes))
                  :wall
                  (merge (assoc moveable-boxes box (move-box box dir)) left-boxes right-boxes)))

              :else moveable-boxes)))))

(defn move-2
  [warehouse]
  (->> (:moves warehouse)
       (reduce (fn [warehouse dir]
                 (let [pos (warehouse :robot)
                       movable-boxes (get-movable-boxes-2 warehouse dir pos)]
                   (if (= movable-boxes :wall)
                     warehouse
                     (-> warehouse
                         (assoc :robot (mapv + dir pos))
                         (update :boxes (fn [boxes] (apply disj boxes (keys movable-boxes))))
                         (update :boxes (fn [boxes] (apply conj boxes (vals movable-boxes))))))))
               warehouse)))

(defn part-2
  {:test (fn []
           (is= (part-2 test-warehouse-2) 9021))}
  [warehouse]
  (->> warehouse
       (move-2)
       (:boxes)
       (map (fn [[[x y] _]] (+ x (* 100 y))))
       (reduce +)))

(comment
  (time (part-1 warehouse))
  ;; "Elapsed time: 45.543791 msecs"
  ;=> 1318523

  (time (part-2 warehouse-2))
  ;; "Elapsed time: 80.074708 msecs"
  ;=> 1337648
  )
