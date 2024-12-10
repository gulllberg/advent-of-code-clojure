(ns advent-of-code.year-2020.day-01)

(def input (slurp "src/advent_of_code/year_2020/inputs/day01.txt"))

; https://adventofcode.com/2020/day/1

(defn solve-a
  []
  (let [numbers (map read-string (clojure.string/split-lines input))
        length (count numbers)]
    (reduce (fn [a i]
              (when-let [v (reduce (fn [aa j]
                                     (when (= 2020 (+ (nth numbers i) (nth numbers j)))
                                       (reduced (* (nth numbers i) (nth numbers j)))))
                                   a
                                   (range (inc i) length))]
                (reduced v)))
            nil
            (range length))))

(comment
  (solve-a)
  ; 32064
  )

(defn solve-b
  []
  (let [numbers (map read-string (clojure.string/split-lines input))
        length (count numbers)]
    (reduce (fn [a i]
              (when-let [v (reduce (fn [aa j]
                                     (when-let [v (reduce (fn [aaa k]
                                                            (when (= 2020 (+ (nth numbers i) (nth numbers j) (nth numbers k)))
                                                              (reduced (* (nth numbers i) (nth numbers j) (nth numbers k)))))
                                                          a
                                                          (range (inc j) length))]
                                       (reduced v)))
                                   a
                                   (range (inc i) length))]
                (reduced v)))
            nil
            (range length))))

(comment
  (solve-b)
  ; 193598720
  )
