(ns advent-of-code.year-2015.day-17
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2015/inputs/day17.txt"))
(def test-input "20\n15\n10\n5\n5")

(defn parse-input
  [input]
  (map read-string (clojure.string/split-lines input)))

(defn get-number-of-combinations
  {:test (fn []
           (is= (get-number-of-combinations (parse-input test-input) 25) 4))}
  [remaining-containers remaining-eggnog]
  (cond (zero? remaining-eggnog) 1
        (neg? remaining-eggnog) 0
        (empty? remaining-containers) 0
        :else (let [[container & remaining-containers] remaining-containers]
                (+ (get-number-of-combinations remaining-containers remaining-eggnog)
                   (get-number-of-combinations remaining-containers (- remaining-eggnog container))))))

(defn part-1
  [input]
  (get-number-of-combinations (parse-input input) 150))

(defn get-number-of-combinations-with-fewest-containers
  {:test (fn []
           (is= (get-number-of-combinations-with-fewest-containers (parse-input test-input) 25 0) [2 3]))}
  [remaining-containers remaining-eggnog containers-used]
  (cond (zero? remaining-eggnog) [containers-used 1]
        (neg? remaining-eggnog) nil
        (empty? remaining-containers) nil
        :else (let [[container & remaining-containers] remaining-containers
                    [cu-1 n1] (get-number-of-combinations-with-fewest-containers remaining-containers remaining-eggnog containers-used)
                    [cu-2 n2] (get-number-of-combinations-with-fewest-containers remaining-containers (- remaining-eggnog container) (inc containers-used))]
                (cond (and (nil? cu-1) (nil? cu-2)) nil
                      (nil? cu-2) [cu-1 n1]
                      (nil? cu-1) [cu-2 n2]
                      (= cu-1 cu-2) [cu-1 (+ n1 n2)]
                      (< cu-1 cu-2) [cu-1 n1]
                      :else [cu-2 n2]))))

(defn part-2
  [input]
  (let [containers (parse-input input)]
    (second (get-number-of-combinations-with-fewest-containers containers 150 0))))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 21.801583 msecs"
  ;=> 1638

  (time (part-2 input))
  ;; "Elapsed time: 31.126917 msecs"
  ;=> 17
  )
