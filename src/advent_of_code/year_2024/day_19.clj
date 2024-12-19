(ns advent-of-code.year-2024.day-19
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2024/inputs/day19.txt"))
(def test-input "r, wr, b, g, bwu, rb, gb, br\n\nbrwrr\nbggr\ngbbr\nrrbgbr\nubwu\nbwurrg\nbrgr\nbbrgwb")

(defn get-towels
  [input]
  (-> input
      (clojure.string/split #"\n\n")
      (first)
      (clojure.string/split #", ")))

(defn get-designs
  [input]
  (-> input
      (clojure.string/split #"\n\n")
      (second)
      (clojure.string/split-lines)))

(def test-towels (get-towels test-input))

(defn get-towels-re-pattern
  [towels]
  (re-pattern (str "^(?:" (->> towels (clojure.string/join "|")) ")+$")))

(def test-towels-re-pattern (get-towels-re-pattern test-towels))

(defn possible-design?
  {:test (fn []
           (is (possible-design? test-towels-re-pattern "brwrr")))}
  [towels-re-pattern design]
  (re-find towels-re-pattern design))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input)
                6))}
  [input]
  (let [designs (get-designs input)
        towels (get-towels input)
        towels-re-pattern (get-towels-re-pattern towels)]
    (->> designs
         (filter (fn [d] (possible-design? towels-re-pattern d)))
         (count))))

(def number-of-different-ways
  (memoize
    (fn [towels design]
      (if (= design "")
        1
        (->> towels
             (filter (fn [t] (clojure.string/starts-with? design t)))
             (map (fn [t] (number-of-different-ways towels (subs design (count t)))))
             (reduce +))))))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input) 16))}
  [input]
  (let [designs (get-designs input)
        towels (get-towels input)
        towels-re-pattern (get-towels-re-pattern towels)]
    (->> designs
         (filter (fn [d] (possible-design? towels-re-pattern d)))
         (map (fn [d] (number-of-different-ways towels d)))
         (reduce +))))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 20.814333 msecs"
  ;=> 324

  (time (part-2 input))
  ;; "Elapsed time: 161.604875 msecs"
  ;=> 575227823167869
  )
