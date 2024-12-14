(ns advent-of-code.year_2018.day_02
  (:require [ysera.collections :refer [seq-contains?]]))

(def input (slurp "src/advent_of_code/year_2018/inputs/day02.txt"))

(defn box-id->count-map
  [box-id]
  (reduce (fn [count-map character]
            (update count-map character (fn [occurrences]
                                          (inc (or occurrences 0)))))
          {}
          box-id))

(defn n-occurrences?
  [count-map n]
  (seq-contains? (vals count-map) n))

(defn count-n-occurrences
  [box-ids n]
  (as-> box-ids $
        (map box-id->count-map $)
        (filter (fn [count-map]
                  (n-occurrences? count-map n))
                $)
        (count $)))

(defn solve-a
  []
  (let [box-ids (clojure.string/split-lines input)
        number-2-occurrences (count-n-occurrences box-ids 2)
        number-3-occurrences (count-n-occurrences box-ids 3)]
    (* number-2-occurrences number-3-occurrences)))

(comment
  (solve-a)
  ;; 8296
  )

(defn get-string-diff
  [s1 s2]
  (reduce (fn [diff index]
            (if (= (nth s1 index) (nth s2 index))
              diff
              (inc diff)))
          0
          (range (count s1))))

(defn get-matching-characters
  [s1 s2]
  (reduce (fn [matching-characters index]
            (if (= (nth s1 index) (nth s2 index))
              (str matching-characters (nth s1 index))
              matching-characters))
          ""
          (range (count s1))))

(defn solve-b
  []
  (let [box-ids (clojure.string/split-lines input)]
    (->> (some (fn [index]
                 (let [box-id-1 (nth box-ids index)]
                   (when-let [box-id-2 (some (fn [box-id-2]
                                               (when (= (get-string-diff box-id-1 box-id-2) 1)
                                                 box-id-2))
                                             (drop (inc index) box-ids))]
                     [box-id-1 box-id-2])))
               (range (count box-ids)))
         (apply get-matching-characters))))

(comment
  (solve-b)
  ;; pazvmqbftrbeosiecxlghkwud
  )
