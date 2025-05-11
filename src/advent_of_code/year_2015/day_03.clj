(ns advent-of-code.year-2015.day-03
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2015/inputs/day03.txt"))
(def test-input "^>v<")

(defn follow-instructions
  [instructions]
  (->> instructions
       (reduce (fn [[visited position] c]
                 (let [dir (case c
                             \< [-1 0]
                             \> [1 0]
                             \^ [0 1]
                             \v [0 -1])
                       position (mapv + position dir)]
                   [(conj visited position) position]))
               [#{[0 0]} [0 0]])
       (first)))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 4))}
  [input]
  (-> (follow-instructions input)
      (count)))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input) 3))}
  [input]
  (let [santa-visited (follow-instructions (take-nth 2 input))
        robo-santa-visited (follow-instructions (take-nth 2 (rest input)))]
    (count (clojure.set/union santa-visited robo-santa-visited))))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 20.168125 msecs"
  ;=> 2592

  (time (part-2 input))
  ;; "Elapsed time: 9.921417 msecs"
  ;=> 2360
  )
