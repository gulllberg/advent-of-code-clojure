(ns advent-of-code.year-2024.day-10
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2024/inputs/day10.txt"))
(def test-input "0123\n1234\n8765\n9876")
(def test-input-large "89010123\n78121874\n87430965\n96549874\n45678903\n32019012\n01329801\n10456732")

(defn create-atlas
  [input]
  (->> input
       (clojure.string/split-lines)
       (into [])
       (reduce-kv (fn [a y row]
                    (->> row
                         (into [])
                         (reduce-kv (fn [a x height]
                                      (assoc a [x y] (read-string (str height))))
                                    a)))
                  {})))

(def atlas (create-atlas input))
(def test-atlas-small (create-atlas test-input))
(def test-atlas-large (create-atlas test-input-large))

(defn get-start-positions
  {:test (fn []
           (is= (get-start-positions test-atlas-small)
                #{[0 0]}))}
  [atlas]
  (reduce-kv (fn [a k v] (if (= v 0) (conj a k) a))
             #{}
             atlas))

(defn create-state
  {:test (fn []
           (is= (create-state test-atlas-small)
                {:hikes {[0 0] #{[0 0]}}
                 :steps 0}))}
  [atlas]
  (let [start-positions (get-start-positions atlas)]
    {:hikes (reduce (fn [a sp] (assoc a sp #{sp}))
                    {}
                    start-positions)
     :steps 0}))
(def directions [[1 0] [0 1] [-1 0] [0 -1]])

(defn next-positions-by-position
  {:test (fn []
           (is= (next-positions-by-position test-atlas-small [0 0] 1)
                #{[1 0] [0 1]}))}
  [atlas p height]
  (->> directions
       (map (fn [d] (map + p d)))
       (filter (fn [np] (= (get atlas np) height)))
       (into #{})))

(defn next-positions-by-positions
  {:test (fn []
           (is= (next-positions-by-positions test-atlas-small #{[1 0] [0 1]} 2)
                #{[1 1] [2 0]}))}
  [atlas ps height]
  (->> ps
       (map (fn [p] (next-positions-by-position atlas p height)))
       (apply clojure.set/union)))

(defn move-a-step
  {:test (fn []
           (is= (move-a-step test-atlas-small (create-state test-atlas-small))
                {:hikes {[0 0] #{[1 0] [0 1]}}
                 :steps 1}))}
  [atlas state]
  (-> state
      (update :steps inc)
      (update :hikes (fn [hikes]
                       (reduce-kv (fn [a start-position boundary]
                                    (assoc a start-position
                                             (next-positions-by-positions atlas
                                                                          boundary
                                                                          (inc (:steps state)))))
                                  {}
                                  hikes)))))

(defn walk
  {:test (fn []
           (is= (walk test-atlas-small) 1)
           (is= (walk test-atlas-large) 36))}
  [atlas]
  (loop [state (create-state atlas)]
    (let [state (move-a-step atlas state)]
      (if (= (:steps state) 9)
        (->> (:hikes state)
             (vals)
             (map count)
             (reduce +))
        (recur state)))))

(defn create-state2
  {:test (fn []
           (is= (create-state2 test-atlas-small)
                {:hikes {[0 0] 1}
                 :steps 0}))}
  [atlas]
  (let [start-positions (get-start-positions atlas)]
    {:hikes (reduce (fn [a sp] (assoc a sp 1))
                    {}
                    start-positions)
     :steps 0}))

(defn move-a-step2
  {:test (fn []
           (is= (move-a-step2 test-atlas-small (create-state2 test-atlas-small))
                {:hikes {[1 0] 1
                         [0 1] 1}
                 :steps 1})
           (is= (move-a-step2 test-atlas-small
                              (move-a-step2 test-atlas-small
                                            (create-state2 test-atlas-small)))
                {:hikes {[1 1] 2
                         [2 0] 1}
                 :steps 2}))}
  [atlas state]
  (-> state
      (update :steps inc)
      (update :hikes (fn [hikes]
                       (reduce-kv (fn [a position n]
                                    (let [next-positions (next-positions-by-position atlas
                                                                                     position
                                                                                     (inc (:steps state)))]
                                      (reduce (fn [a np]
                                                (update a np
                                                        (fn [m] (+ n (or m 0)))))
                                              a
                                              next-positions)))
                                  {}
                                  hikes)))))

(defn walk-2
  {:test (fn []
           (is= (walk-2 test-atlas-large) 81))}
  [atlas]
  (loop [state (create-state2 atlas)]
    (let [state (move-a-step2 atlas state)]
      (if (= (:steps state) 9)
        (->> (:hikes state)
             (vals)
             (reduce +))
        (recur state)))))

(comment
  ;; "Elapsed time: 11.151 msecs"
  ;=> 667
  (time (walk atlas))

  ;; "Elapsed time: 6.763667 msecs"
  ;=> 1344
  (time (walk atlas))
  )
