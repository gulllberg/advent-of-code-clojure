(ns advent-of-code.year-2021.day-22
  (:require [ysera.test :refer [is= is is-not]]
            [clojure.math.combinatorics :as combo]))

(def input (slurp "src/advent_of_code/year_2021/inputs/day22.txt"))

(defn parse-line
  {:test (fn []
           (is= (parse-line "on x=-3..43,y=-28..22,z=-6..38")
                {:command :on :x [-3 43] :y [-28 22] :z [-6 38]}))}
  [line]
  (let [[x y z] (->> line
                     (re-seq #"-?\d+")
                     (map read-string)
                     (partition 2))]
    {:command (if (clojure.string/starts-with? line "on") :on :off)
     :x       x
     :y       y
     :z       z}))

(defn reboot-sequence
  [input only-small-region]
  (let [lines (clojure.string/split-lines input)]
    (reduce (fn [on-positions {command :command x :x y :y z :z}]
              (let [operation (if (= command :on) conj disj)]
                (reduce (fn [on-positions position]
                          (operation on-positions position))
                        on-positions
                        (combo/cartesian-product (if only-small-region (range (max (first x) -50) (inc (min (second x) 50)))
                                                                       (range (first x) (inc (second x))))
                                                 (if only-small-region (range (max (first y) -50) (inc (min (second y) 50)))
                                                                       (range (first y) (inc (second y))))
                                                 (if only-small-region (range (max (first z) -50) (inc (min (second z) 50)))
                                                                       (range (first z) (inc (second z))))))))
            #{}
            (map parse-line lines))))

(defn part-1
  []
  (count (reboot-sequence input true)))

(comment
  (time (part-1))
  ; "Elapsed time: 2997.809172 msecs"
  ; 648023
  )

(defn part-2
  []
  (count (reboot-sequence input false)))

(comment
  (part-2)
  ; too slow
  )
