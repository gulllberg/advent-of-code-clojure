(ns advent-of-code.year-2025.day-02
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2025/inputs/day02.txt"))
(def test-input "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124")

(defn invalid-id?
  {:test (fn []
           (is (invalid-id? "11"))
           (is (invalid-id? "1010"))
           (is (invalid-id? "1188511885"))
           (is-not (invalid-id? "123")))}
  [id]
  (re-find #"^(\d+)\1$" id))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 1227775554))}
  [input]
  (->> (clojure.string/split input #",")
       (mapcat (fn [id-range]
                 (let [[start end] (map read-string (re-seq #"\d+" id-range))]
                   (->> (range start (inc end))
                        (filter (fn [n]
                                  (invalid-id? (str n))))))))
       (reduce +)))

(defn invalid-id?-2
  {:test (fn []
           (is (invalid-id?-2 "11"))
           (is (invalid-id?-2 "1010"))
           (is (invalid-id?-2 "1188511885"))
           (is (invalid-id?-2 "999"))
           (is (invalid-id?-2 "1111111"))
           (is (invalid-id?-2 "1212121212"))
           (is-not (invalid-id?-2 "123")))}
  [id]
  (re-find #"^(\d+)\1+$" id))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input) 4174379265))}
  [input]
  (->> (clojure.string/split input #",")
       (mapcat (fn [id-range]
                 (let [[start end] (map read-string (re-seq #"\d+" id-range))]
                   (->> (range start (inc end))
                        (filter (fn [n]
                                  (invalid-id?-2 (str n))))))))
       (reduce +)))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 280.896334 msecs"
  ;=> 17077011375

  (time (part-2 input))
  ;; "Elapsed time: 279.128 msecs"
  ;=> 36037497037
  )
