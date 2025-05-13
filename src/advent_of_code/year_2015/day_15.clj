(ns advent-of-code.year-2015.day-15
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2015/inputs/day15.txt"))
(def test-input "Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8\nCinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3")

(defn parse-input
  [input]
  (reduce (fn [a line]
            (conj a (map read-string (re-seq #"-?\d+" line))))
          []
          (clojure.string/split-lines input)))

(defn vector-multiply
  [u s]
  (map (fn [x] (* x s)) u))

(defn get-best-score
  [remaining-ingredients remaining-teaspoons values]
  (let [[ingredient & remaining-ingredients] remaining-ingredients]
    (if (empty? remaining-ingredients)
      (->> (vector-multiply ingredient remaining-teaspoons)
           (mapv + values)
           (map (fn [v] (max v 0)))
           (reduce *))
      (apply max (map (fn [tsp]
                        (get-best-score remaining-ingredients (- remaining-teaspoons tsp) (->> (vector-multiply ingredient tsp)
                                                                                               (mapv + values))))
                      (range (inc remaining-teaspoons)))))))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 62842880))}
  [input]
  (let [ingredients (map drop-last (parse-input input))]
    (get-best-score ingredients 100 [0 0 0 0])))

(defn get-best-score-500-calories
  [remaining-ingredients remaining-teaspoons values]
  (let [[ingredient & remaining-ingredients] remaining-ingredients]
    (if (empty? remaining-ingredients)
      (let [values (->> (vector-multiply ingredient remaining-teaspoons)
                        (mapv + values)
                        (map (fn [v] (max v 0))))]
        (if (= 500 (last values))
          (reduce * (drop-last values))
          ##-Inf))
      (apply max (map (fn [tsp]
                        (get-best-score-500-calories remaining-ingredients (- remaining-teaspoons tsp) (->> (vector-multiply ingredient tsp)
                                                                                                            (mapv + values))))
                      (range (inc remaining-teaspoons)))))))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input) 57600000))}
  [input]
  (get-best-score-500-calories (parse-input input) 100 [0 0 0 0 0]))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 423.834417 msecs"
  ;=> 13882464

  (time (part-2 input))
  ;; "Elapsed time: 385.9715 msecs"
  ;=> 11171160
  )
