(ns advent-of-code.year-2015.day-21
  (:require [ysera.test :refer [is= is is-not]]
            [clojure.math.combinatorics :as combo]))

(def input (slurp "src/advent_of_code/year_2015/inputs/day21.txt"))

;; [cost damage armour]
(def weapons [[8 4 0] [10 5 0] [25 6 0] [40 7 0] [74 8 0]])
(def armours [[13 0 1] [31 0 2] [53 0 3] [75 0 4] [102 0 5]])
(def rings [[25 1 0] [50 2 0] [100 3 0] [20 0 1] [40 0 2] [80 0 3]])

;; 1 weapon, 0-1 armour, 0-2 rings
(defn get-all-gear-combinations
  []
  (let [weapon-options weapons
        armour-options (conj armours [0 0 0])
        ring-options (concat rings [[0 0 0]] (map (fn [[r1 r2]]
                                                     ;; treat as one "combined ring"
                                                     (mapv + r1 r2))
                                                   (combo/combinations rings 2)))]
    (combo/cartesian-product weapon-options armour-options ring-options)))

(defn damage-dealt
  {:test (fn []
           (is= (damage-dealt 8 3) 5)
           (is= (damage-dealt 8 300) 1))}
  [damage armour]
  (max 1 (- damage armour)))

(defn player-wins?
  {:test (fn []
           (is (player-wins? [5 5 8] [7 2 12])))}
  [player boss]
  (let [[player-damage player-armour player-health] player
        [boss-damage boss-armour boss-health] boss
        player-damage-dealt (damage-dealt player-damage boss-armour)
        boss-damage-dealt (damage-dealt boss-damage player-armour)
        turns-to-kill-boss (Math/ceil (/ boss-health player-damage-dealt))
        turns-to-die (Math/ceil (/ player-health boss-damage-dealt))]
    (>= turns-to-die turns-to-kill-boss)))

(defn part-1
  [input]
  (let [[boss-health boss-damage boss-armour] (map read-string (re-seq #"\d+" input))
        gear-combinations (->> (get-all-gear-combinations)
                               (map (fn [[w a r]]
                                      (mapv + w a r))))
        sorted-gear-combinations (sort-by first gear-combinations)]
    (->> sorted-gear-combinations
         (filter (fn [[_ player-damage player-armour]]
                   (player-wins? [player-damage player-armour 100] [boss-damage boss-armour boss-health])))
         (ffirst))))

(defn part-2
  [input]
  (let [[boss-health boss-damage boss-armour] (map read-string (re-seq #"\d+" input))
        gear-combinations (->> (get-all-gear-combinations)
                               (map (fn [[w a r]]
                                      (mapv + w a r))))
        sorted-gear-combinations (reverse (sort-by first gear-combinations))]
    (->> sorted-gear-combinations
         (remove (fn [[_ player-damage player-armour]]
                   (player-wins? [player-damage player-armour 100] [boss-damage boss-armour boss-health])))
         (ffirst))))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 7.399333 msecs"
  ;=> 78

  (time (part-2 input))
  ;; "Elapsed time: 5.918083 msecs"
  ;=> 148
  )