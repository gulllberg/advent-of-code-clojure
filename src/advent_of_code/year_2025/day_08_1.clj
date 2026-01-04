(ns advent-of-code.year-2025.day-08
  (:require [ysera.test :refer [is= is is-not]]
            [clojure.math.combinatorics]))

;; With Tomas

(def input (slurp "src/advent_of_code/year_2025/inputs/day08.txt"))
(def test-input "162,817,812\n57,618,57\n906,360,560\n592,479,940\n352,342,300\n466,668,158\n542,29,236\n431,825,988\n739,650,466\n52,470,668\n216,146,977\n819,987,18\n117,168,530\n805,96,715\n346,949,466\n970,615,88\n941,993,340\n862,61,35\n984,92,344\n425,690,689")

(defn parse-input
  {:test (fn []
           (is= (parse-input test-input)
                #{{[162 817 812] #{}}
                  {[57 618 57] #{}}
                  {[906 360 560] #{}}
                  {[592 479 940] #{}}
                  {[352 342 300] #{}}
                  {[466 668 158] #{}}
                  {[542 29 236] #{}}
                  {[431 825 988] #{}}
                  {[739 650 466] #{}}
                  {[52 470 668] #{}}
                  {[216 146 977] #{}}
                  {[819 987 18] #{}}
                  {[117 168 530] #{}}
                  {[805 96 715] #{}}
                  {[346 949 466] #{}}
                  {[970 615 88] #{}}
                  {[941 993 340] #{}}
                  {[862 61 35] #{}}
                  {[984 92 344] #{}}
                  {[425 690 689] #{}}}))}
  [input]
  (->> (clojure.string/split-lines input)
       (map (fn [line]
              {(mapv read-string (re-seq #"\d+" line)) #{}}))
       (into #{})))

(declare get-distance-squared-between-junction-boxes)
(defn get-distance-squared-between-junction-boxes-raw
  {:test (fn []
           (is= (get-distance-squared-between-junction-boxes [1 1 1] [3 3 3]) 12))}
  [j1 j2]
  (->> (map - j1 j2)
       (map (fn [d] (* d d)))
       (reduce +)))
(def get-distance-squared-between-junction-boxes (memoize get-distance-squared-between-junction-boxes-raw))

;(defn get-distance-squared-between-circuits
;  {:test (fn []
;           (is= (get-distance-squared-between-circuits {[0 0 0] [1 1 1]} #{[3 3 3]})
;                12))}
;  [c1 c2]
;  (->> (for [j1 c1
;             j2 c2]
;         (get-distance-squared-between-junction-boxes j1 j2))
;       (apply min)))

(defn get-distance-and-closest-junction-boxes-between-circuits
  [c1 c2]
  (->> (for [j1 (keys c1)
             j2 (keys c2)
             :when (and (not (contains? (get c1 j1) j2))
                        (not= j1 j2))]
         [(get-distance-squared-between-junction-boxes j1 j2) j1 j2])
       (sort-by first)
       (first)))

(defn connect-closest-circuits
  {:test (fn []
           (is= (connect-closest-circuits (parse-input test-input))
                #{{[162 817 812] #{[425 690 689]}
                   [425 690 689] #{[162 817 812]}}
                  {[57 618 57] #{}}
                  {[906 360 560] #{}}
                  {[592 479 940] #{}}
                  {[352 342 300] #{}}
                  {[466 668 158] #{}}
                  {[542 29 236] #{}}
                  {[431 825 988] #{}}
                  {[739 650 466] #{}}
                  {[52 470 668] #{}}
                  {[216 146 977] #{}}
                  {[819 987 18] #{}}
                  {[117 168 530] #{}}
                  {[805 96 715] #{}}
                  {[346 949 466] #{}}
                  {[970 615 88] #{}}
                  {[941 993 340] #{}}
                  {[862 61 35] #{}}
                  {[984 92 344] #{}}}))}
  [circuits]
  (let [[c1 j1 c2 j2]
        (loop [[c & cs :as all-cs] circuits
               min-distance-squared ##Inf
               min-c1 nil
               min-j1 nil
               min-c2 nil
               min-j2 nil]
          (if (nil? c)
            [min-c1 min-j1 min-c2 min-j2]
            (let [[min-distance-squared min-c1 min-j1 min-c2 min-j2]
                  (loop [[cc & ccs] all-cs
                         min-distance-squared min-distance-squared
                         min-c1 min-c1
                         min-j1 min-j1
                         min-c2 min-c2
                         min-j2 min-j2]
                    (if (nil? cc)
                      [min-distance-squared min-c1 min-j1 min-c2 min-j2]
                      (let [[distance-squared j1 j2] (get-distance-and-closest-junction-boxes-between-circuits c cc)]
                        (if (and distance-squared (< distance-squared min-distance-squared))
                          (recur ccs distance-squared c j1 cc j2)
                          (recur ccs min-distance-squared min-c1 min-j1 min-c2 min-j2)))))]
              (recur cs min-distance-squared min-c1 min-j1 min-c2 min-j2))))]
    (-> circuits
        (disj c1 c2)
        (conj (-> (merge c1 c2)
                  (update j1 conj j2)
                  (update j2 conj j1)))))

  ;(let [[_ c1 c2] (->> (clojure.math.combinatorics/combinations circuits 2)
  ;                     (reduce (fn [[min-distance-squared _ _ :as a] [c1 c2]]
  ;                               (let [distance-squared (get-distance-squared-between-circuits c1 c2)]
  ;                                 (if (< distance-squared min-distance-squared)
  ;                                   [distance-squared c1 c2]
  ;                                   a)))
  ;                             [##Inf nil nil]))]
  ;  (println c1 c2)
  ;  (-> circuits
  ;      (disj c1 c2)
  ;      (conj (clojure.set/union c1 c2))))
  )

(defn part-1
  {:test (fn []
           (is= (part-1 test-input 10) 40))}
  [input number-of-connections]
  (->> (range number-of-connections)
       (reduce (fn [circuits index]
                 (println index)
                 (connect-closest-circuits circuits))
               (parse-input input))
       (map count)
       (sort)
       (take-last 3)
       (apply *)))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input) 25272))}
  [input]
  )

(comment
  (time (part-1 input 1000))
  ;; "Elapsed time: 400162.069084 msecs"
  ;=> 115885

  (time (part-2 input))
  ;;
  )
