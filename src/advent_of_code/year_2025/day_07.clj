(ns advent-of-code.year-2025.day-07
  (:require [ysera.test :refer [is= is is-not]]))

;; With Tomas

(def input (slurp "src/advent_of_code/year_2025/inputs/day07.txt"))
(def test-input ".......S.......\n...............\n.......^.......\n...............\n......^.^......\n...............\n.....^.^.^.....\n...............\n....^.^...^....\n...............\n...^.^...^.^...\n...............\n..^...^.....^..\n...............\n.^.^.^.^.^...^.\n...............")

(defn move-down-a-step
  {:test (fn []
           (is= (move-down-a-step (clojure.string/split-lines test-input) 0 #{7}) [#{7} 0])
           (is= (move-down-a-step (clojure.string/split-lines test-input) 1 #{7}) [#{6 8} 1])
           (is= (move-down-a-step (clojure.string/split-lines test-input) 2 #{6 8}) [#{6 8} 0])
           (is= (move-down-a-step (clojure.string/split-lines test-input) 3 #{6 8}) [#{5 7 9} 2]))}
  [tachyon-manifold index positions]
  (->> positions
       (reduce (fn [a p]
                 (let [c (get-in tachyon-manifold [(inc index) p])]
                   (if (= c \.)
                     (update a 0 conj p)
                     (-> a
                         (update 0 conj (dec p) (inc p))
                         (update 1 inc)))))
               [#{} 0])))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 21))}
  [input]
  (let [tachyon-manifold (clojure.string/split-lines input)
        start-position (->> (first tachyon-manifold)
                            (map-indexed (fn [index item] [index item]))
                            (some (fn [[index item]] (when (= item \S) index))))]
    (->> (range (dec (count tachyon-manifold)))
         (reduce (fn [[positions splits] index]
                   (let [[positions new-splits] (move-down-a-step tachyon-manifold index positions)]
                     [positions (+ splits new-splits)]))
                 [#{start-position} 0])
         (second))))

(defn move-down-a-step-2
  {:test (fn []
           (is= (move-down-a-step-2 (clojure.string/split-lines test-input) 0 {7 1}) {7 1})
           (is= (move-down-a-step-2 (clojure.string/split-lines test-input) 1 {7 1}) {6 1 8 1})
           (is= (move-down-a-step-2 (clojure.string/split-lines test-input) 2 {6 1 8 1}) {6 1 8 1})
           (is= (move-down-a-step-2 (clojure.string/split-lines test-input) 3 {6 1 8 1}) {5 1 7 2 9 1}))}
  [tachyon-manifold index positions]
  (->> positions
       (reduce-kv (fn [a p n]
                    (let [c (get-in tachyon-manifold [(inc index) p])]
                      (if (= c \.)
                        (update a p (fn [m] (+ n (or m 0))))
                        (-> a
                            (update (dec p) (fn [m] (+ n (or m 0))))
                            (update (inc p) (fn [m] (+ n (or m 0))))))))
                  {})))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input) 40))}
  [input]
  (let [tachyon-manifold (clojure.string/split-lines input)
        start-position (->> (first tachyon-manifold)
                            (map-indexed (fn [index item] [index item]))
                            (some (fn [[index item]] (when (= item \S) index))))]
    (->> (range (dec (count tachyon-manifold)))
         (reduce (fn [positions v]
                   (move-down-a-step-2 tachyon-manifold v positions))
                 {start-position 1})
         (vals)
         (reduce + 0))))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 12.139083 msecs"
  ;=> 1662

  (time (part-2 input))
  ;; "Elapsed time: 6.693209 msecs"
  ;=> 40941112789504
  )
