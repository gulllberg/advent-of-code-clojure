(ns advent-of-code.year-2025.day-08
  (:require [ysera.test :refer [is= is is-not]]
            [clojure.math.combinatorics]))

;; With Tomas

(def input (slurp "src/advent_of_code/year_2025/inputs/day08.txt"))
(def test-input "162,817,812\n57,618,57\n906,360,560\n592,479,940\n352,342,300\n466,668,158\n542,29,236\n431,825,988\n739,650,466\n52,470,668\n216,146,977\n819,987,18\n117,168,530\n805,96,715\n346,949,466\n970,615,88\n941,993,340\n862,61,35\n984,92,344\n425,690,689")

(defn parse-input
  {:test (fn []
           (is= (parse-input test-input)
                [[162 817 812]
                 [57 618 57]
                 [906 360 560]
                 [592 479 940]
                 [352 342 300]
                 [466 668 158]
                 [542 29 236]
                 [431 825 988]
                 [739 650 466]
                 [52 470 668]
                 [216 146 977]
                 [819 987 18]
                 [117 168 530]
                 [805 96 715]
                 [346 949 466]
                 [970 615 88]
                 [941 993 340]
                 [862 61 35]
                 [984 92 344]
                 [425 690 689]]))}
  [input]
  (->> (clojure.string/split-lines input)
       (map (fn [line]
              (mapv read-string (re-seq #"\d+" line))))))

(defn create-circuits
  {:test (fn []
           (is= (create-circuits (parse-input test-input))
                {[162 817 812] #{[162 817 812]}
                 [57 618 57]   #{[57 618 57]}
                 [906 360 560] #{[906 360 560]}
                 [592 479 940] #{[592 479 940]}
                 [352 342 300] #{[352 342 300]}
                 [466 668 158] #{[466 668 158]}
                 [542 29 236]  #{[542 29 236]}
                 [431 825 988] #{[431 825 988]}
                 [739 650 466] #{[739 650 466]}
                 [52 470 668]  #{[52 470 668]}
                 [216 146 977] #{[216 146 977]}
                 [819 987 18]  #{[819 987 18]}
                 [117 168 530] #{[117 168 530]}
                 [805 96 715]  #{[805 96 715]}
                 [346 949 466] #{[346 949 466]}
                 [970 615 88]  #{[970 615 88]}
                 [941 993 340] #{[941 993 340]}
                 [862 61 35]   #{[862 61 35]}
                 [984 92 344]  #{[984 92 344]}
                 [425 690 689] #{[425 690 689]}}))}
  [junction-boxes]
  (->> junction-boxes
       (reduce (fn [a v]
                 (assoc a v #{v}))
               {})))

(defn get-distance-squared-between-junction-boxes
  {:test (fn []
           (is= (get-distance-squared-between-junction-boxes [1 1 1] [3 3 3]) 12))}
  [j1 j2]
  (->> (map - j1 j2)
       (map (fn [d] (* d d)))
       (reduce +)))

(defn get-sorted-distances
  {:test (fn []
           (is= (->> (parse-input test-input)
                     (get-sorted-distances)
                     (take 3)
                     (map rest))
                [[[162, 817, 812] [425, 690, 689]]
                 [[162, 817, 812] [431, 825, 988]]
                 [[906, 360, 560] [805, 96, 715]]]))}
  [junction-boxes]
  (->> (clojure.math.combinatorics/combinations junction-boxes 2)
       (map (fn [[j1 j2]]
              [(get-distance-squared-between-junction-boxes j1 j2) j1 j2]))
       (sort-by first)))

(defn connect-junction-boxes
  {:test (fn []
           (is= (connect-junction-boxes {[862 61 35]   #{[862 61 35] }
                                         [984 92 344]  #{[984 92 344]}
                                         [425 690 689] #{[425 690 689]}}
                                        [862 61 35]
                                        [984 92 344])
                {[862 61 35]   #{[862 61 35] [984 92 344]}
                 [984 92 344]  #{[862 61 35] [984 92 344]}
                 [425 690 689] #{[425 690 689]}})
           (is= (connect-junction-boxes {[57 618 57]   #{[57 618 57]}
                                         [862 61 35]   #{[862 61 35] [984 92 344]}
                                         [984 92 344]  #{[862 61 35] [984 92 344]}
                                         [425 690 689] #{[425 690 689]}}
                                        [862 61 35]
                                        [57 618 57])
                {[57 618 57]   #{[57 618 57] [862 61 35] [984 92 344]}
                 [862 61 35]   #{[57 618 57] [862 61 35] [984 92 344]}
                 [984 92 344]  #{[57 618 57] [862 61 35] [984 92 344]}
                 [425 690 689] #{[425 690 689]}}))}
  [circuits j1 j2]
  (let [new-circuit (clojure.set/union (get circuits j1) (get circuits j2))]
    (reduce (fn [circuits junction-box]
              (assoc circuits junction-box new-circuit))
            circuits
            new-circuit)))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input 10) 40))}
  [input number-of-connections]
  (let [junction-boxes (parse-input input)
        circuits (create-circuits junction-boxes)
        connections-to-make (take number-of-connections (get-sorted-distances junction-boxes))]
    (->> connections-to-make
         (reduce (fn [circuits [_ j1 j2]]
                   (connect-junction-boxes circuits j1 j2))
                 circuits)
         (vals)
         (into #{})
         (map count)
         (sort)
         (take-last 3)
         (reduce *))))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input) 25272))}
  [input]
  (let [junction-boxes (parse-input input)
        circuits (create-circuits junction-boxes)
        connections-to-make (get-sorted-distances junction-boxes)
        number-of-junction-boxes (count junction-boxes)]
    (loop [circuits circuits
           [[_ j1 j2] & connections-to-make] connections-to-make]
      (let [circuits (connect-junction-boxes circuits j1 j2)]
        (if (= (count (get circuits j1)) number-of-junction-boxes)
          (* (first j1) (first j2))
          (recur circuits connections-to-make))))))

(comment
  (time (part-1 input 1000))
  ; "Elapsed time: 1451.779083 msecs"
  ;=> 115885

  (time (part-2 input))
  ; "Elapsed time: 1570.411167 msecs"
  ;=> 274150525
  )
