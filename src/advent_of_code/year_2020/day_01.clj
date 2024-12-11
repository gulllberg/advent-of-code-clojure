(ns advent-of-code.year-2020.day-01)

(def input (slurp "src/advent_of_code/year_2020/inputs/day01.txt"))

(defn part-1
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
  (time (part-1))
  ; 32064
  ; "Elapsed time: 33.021625 msecs"
  )

(defn part-2
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
  (time (part-2))
  ; 193598720
  ; "Elapsed time: 512.604916 msecs"
  )
