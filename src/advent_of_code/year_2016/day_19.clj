(ns advent-of-code.year-2016.day-19
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2016/inputs/day19.txt"))
(def test-input "5")

(defn one-round
  [elves]
  (let [res (keep-indexed (fn [index elf]
                            (when (even? index)
                              elf))
                          elves)]
    (if (odd? (count elves))
      (drop 1 res)
      res)))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 3))}
  [input]
  (let [n-elves (read-string input)
        starting-elves (range 1 (inc n-elves))]
    (loop [elves starting-elves]
      (if (= 1 (count elves))
        (first elves)
        (recur (one-round elves))))))

(defn get-removed-indices
  {:test (fn []
           (is= (get-removed-indices 5) #{2 4 0})
           (is= (get-removed-indices 14) #{7 8 10 11 13 0 2 3 5})
           (is= (get-removed-indices 7) #{3 5 6 1})
           (is= (get-removed-indices 9) #{4 6 7 0 1 3}))}
  [number-of-elves]
  (let [start (if (odd? number-of-elves) (/ (dec number-of-elves) 2) (/ number-of-elves 2))
        skip-offset (if (odd? number-of-elves) 1 2)
        first-half-length (quot number-of-elves 2)
        second-half-length (->> (range start number-of-elves)
                                (keep (fn [index]

                                        (when (= skip-offset (mod (- index start) 3))
                                          index)))
                                (count))
        length (+ first-half-length second-half-length)]
    (into #{}
          (take length (keep-indexed (fn [index n]
                                       (when (not= skip-offset (mod index 3))
                                         (mod n number-of-elves)))
                                     (range start (+ start (* 3 length))))))))

(defn one-round-2
  [elves]
  (let [removed-indices (get-removed-indices (count elves))]
    (keep-indexed (fn [index n]
                    (when-not (contains? removed-indices index)
                      n))
                  elves)))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input) 2)
           (is= (part-2 "14") 5)
           (is= (part-2 "6") 3)
           (is= (part-2 "7") 5))}
  [input]
  (let [n-elves (read-string input)
        starting-elves (range 1 (inc n-elves))]
    (loop [elves starting-elves]
      (if (= 1 (count elves))
        (first elves)
        (recur (one-round-2 elves))))))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 406.604833 msecs"
  ;=> 1808357

  (time (part-2 input))
  ;; "Elapsed time: 3101.338 msecs"
  ;=> 1407007
  )
