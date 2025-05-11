(ns advent-of-code.year-2015.day-06
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2015/inputs/day06.txt"))

(defn update-lights
  [state [x1 y1 x2 y2] reduce-fn]
  (->> (for [x (range x1 (inc x2))
             y (range y1 (inc y2))]
         [x y])
       (reduce reduce-fn state)))

(defn turn-on
  [state coords]
  (update-lights state coords (fn [state p] (conj state p)) ))

(defn turn-off
  [state coords]
  (update-lights state coords (fn [state p] (disj state p))))

(defn toggle
  [state coords]
  (update-lights state coords (fn [state p] (if (contains? state p) (disj state p) (conj state p)))))

(defn do-instruction
  [state line]
  (let [fn-to-use (cond (clojure.string/starts-with? line "turn on") turn-on
                        (clojure.string/starts-with? line "turn off") turn-off
                        (clojure.string/starts-with? line "toggle") toggle)
        numbers (map read-string (re-seq #"\d+" line))]
    (fn-to-use state numbers)))

(defn part-1
  [input]
  (->> (clojure.string/split-lines input)
       (reduce do-instruction #{})
       (count)))

(defn turn-on-2
  [state coords]
  (update-lights state coords (fn [state p] (update state p (fn [v] (if v (inc v) 1))))))

(defn turn-off-2
  [state coords]
  (update-lights state coords (fn [state p] (update state p (fn [v] (if (and v (> v 0)) (dec v) 0))))))

(defn toggle-2
  [state coords]
  (update-lights state coords (fn [state p] (update state p (fn [v] (if v (+ v 2) 2))))))

(defn do-instruction-2
  [state line]
  (let [fn-to-use (cond (clojure.string/starts-with? line "turn on") turn-on-2
                        (clojure.string/starts-with? line "turn off") turn-off-2
                        (clojure.string/starts-with? line "toggle") toggle-2)
        numbers (map read-string (re-seq #"\d+" line))]
    (fn-to-use state numbers)))

(defn part-2
  [input]
  (->> (clojure.string/split-lines input)
       (reduce do-instruction-2 {})
       (vals)
       (reduce +)))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 9775.488667 msecs"
  ;=> 543903

  (time (part-2 input))
  ;; "Elapsed time: 13857.069625 msecs"
  ;=> 14687245
  )
