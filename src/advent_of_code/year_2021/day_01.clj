(ns advent-of-code.year-2021.day-01
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2021/inputs/day01.txt"))

(defn sonar-sweep
  {:test (fn []
           (is= (sonar-sweep [199 200 208 210 200 207 240 269 260 263]) 7)
           (is= (sonar-sweep [199 200 208 210 200 207 240 269 260 263] 3) 5))}
  ([numbers gap]
   (reduce (fn [a i]
             (+ a
                (if (< (nth numbers i) (nth numbers (+ i gap)))
                  1
                  0)))
           0
           (range (- (count numbers) gap))))
  ([numbers] (sonar-sweep numbers 1)))

(defn part-1
  []
  (sonar-sweep (map read-string (clojure.string/split-lines input))))

(comment
  (time (part-1))
  ; 1121
  ; "Elapsed time: 54.487875 msecs"
  )

(defn part-2
  []
  (sonar-sweep (map read-string (clojure.string/split-lines input)) 3))

(comment
  (time (part-2))
  ; 1065
  ; "Elapsed time: 48.635958 msecs"
  )
