(ns advent-of-code.year-2025.day-05
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2025/inputs/day05.txt"))
(def test-input "3-5\n10-14\n16-20\n12-18\n\n1\n5\n8\n11\n17\n32")

(defn parse-input
  {:test (fn []
           (is= (parse-input test-input) [[[3 5] [10 14] [16 20] [12 18]] [1 5 8 11 17 32]]))}
  [input]
  (let [[fresh-ranges-part available-ids-part] (clojure.string/split input #"\n\n")
        fresh-ranges (->> (clojure.string/split-lines fresh-ranges-part)
                          (map (fn [line]
                                 (map read-string (re-seq #"\d+" line)))))
        available-ids (->> (clojure.string/split-lines available-ids-part)
                           (map read-string))]
    [fresh-ranges available-ids]))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 3))}
  [input]
  (let [[fresh-ranges available-ids] (parse-input input)]
    (->> available-ids
         (filter (fn [id]
                   (some (fn [[start end]]
                           (<= start id end))
                         fresh-ranges)))
         (count))))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input) 14))}
  [input]
  (let [[fresh-ranges _] (parse-input input)
        fresh-ranges (sort-by first fresh-ranges)]
    (loop [[fresh-range & fresh-ranges] fresh-ranges
           max-handled 0
           total-valid 0]
      (if (nil? fresh-range)
        total-valid
        (let [[range-start range-end] fresh-range]
          (if (<= range-end max-handled)
            (recur fresh-ranges max-handled total-valid)
            (let [start (max range-start (inc max-handled))
                  end range-end
                  valid (inc (- end start))]
              (recur fresh-ranges range-end (+ total-valid valid)))))))))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 25.633625 msecs"
  ;=> 567

  (time (part-2 input))
  ;; "Elapsed time: 1.068 msecs"
  ;=> 354149806372909
  )
