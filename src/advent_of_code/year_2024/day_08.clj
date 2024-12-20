(ns advent-of-code.year-2024.day-08
  (:require [ysera.test :refer [is= is is-not]]
            [clojure.math.combinatorics :as combo]))

(def input (slurp "src/advent_of_code/year_2024/inputs/day08.txt"))
(def test-input "............\n........0...\n.....0......\n.......0....\n....0.......\n......A.....\n............\n............\n........A...\n.........A..\n............\n............")

(defn create-state
  {:test (fn []
           (is= (create-state test-input)
                {:i-max    11
                 :j-max    11
                 :antennae {\0 #{[2 5] [1 8] [4 4] [3 7]}
                            \A #{[8 8] [9 9] [5 6]}}}))}
  [input]
  (->> input
       (clojure.string/split-lines)
       (into [])
       (reduce-kv (fn [a i line]
                    (->> line
                         (into [])
                         (reduce-kv (fn [a j c]
                                      (if (= c \.)
                                        (-> a
                                            (update :i-max max i)
                                            (update :j-max max j))
                                        (-> a
                                            (update :i-max max i)
                                            (update :j-max max j)
                                            (update-in [:antennae c] (fn [old]
                                                                       (if (nil? old)
                                                                         #{[i j]}
                                                                         (conj old [i j])))))))
                                    a)))
                  {:i-max    0
                   :j-max    0
                   :antennae {}})))

(defn get-antinodes
  {:test (fn []
           (is= (get-antinodes #{[0 0] [1 1]})
                #{[2 2] [-1 -1]})
           (is= (get-antinodes #{[0 0] [1 1] [1 2]})
                #{[2 2] [-1 -1]
                  [2 4] [-1 -2]
                  [1 3] [1 0]}))}
  [positions]
  (->> (combo/combinations positions 2)
       (reduce (fn [acc [p1 p2]]
                 (let [[i1 j1] p1
                       [i2 j2] p2
                       di (- i2 i1)
                       dj (- j2 j1)
                       a1 [(+ i2 di) (+ j2 dj)]
                       a2 [(- i1 di) (- j1 dj)]]
                   (conj acc a1 a2)))
               #{})))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 14))}
  [input]
  (let [state (create-state input)]
    (->> (:antennae state)
         (vals)
         (reduce (fn [a positions]
                   (->> positions
                        (get-antinodes)
                        (filter (fn [[i j]]
                                  (and (<= 0 i (:i-max state))
                                       (<= 0 j (:j-max state)))))
                        (reduce conj a)))
                 #{})
         (count))))

(defn get-antinodes-in-direction
  [position direction i-max j-max]
  (loop [result #{position}
         position position]
    (let [new-position (map + position direction)
          [new-i new-j] new-position]
      (if-not (and (<= 0 new-i i-max)
                   (<= 0 new-j j-max))
        result
        (recur (conj result new-position)
               new-position)))))

(defn get-antinodes-2
  {:test (fn []
           (is= (get-antinodes-2 #{[0 0] [1 1]} 4 4)
                #{[0 0] [1 1] [2 2] [3 3] [4 4]}))}
  [positions i-max j-max]
  (->> (combo/combinations positions 2)
       (reduce (fn [acc [p1 p2]]
                 (let [[i1 j1] p1
                       [i2 j2] p2
                       di (- i2 i1)
                       dj (- j2 j1)
                       antinodes-d1 (get-antinodes-in-direction p2 [di dj] i-max j-max)
                       antinodes-d2 (get-antinodes-in-direction p1 [(- di) (- dj)] i-max j-max)]
                   (clojure.set/union acc antinodes-d1 antinodes-d2)))
               #{})))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input) 34))}
  [input]
  (let [state (create-state input)]
    (->> (:antennae state)
         (vals)
         (reduce (fn [a positions]
                   (clojure.set/union a (get-antinodes-2 positions (:i-max state) (:j-max state))))
                 #{})
         (count))))

(comment
  ;; "Elapsed time: 4.961416 msecs"
  ;=> 371
  (time (part-1 input))

  ;; "Elapsed time: 11.508583 msecs"
  ;=> 1229
  (time (part-2 input))
  )
