(ns advent-of-code.year-2019.day-02
  (:require [ysera.test :refer [is= is is-not]]
            [advent-of-code.year-2019.intcode :refer [parse-program run-intcode-program]]))

(def input (slurp "src/advent_of_code/year_2019/inputs/day02.txt"))

(defn part-1
  [input]
  (-> (parse-program input)
      (assoc 1 12 2 2)
      (run-intcode-program)
      (:memory)
      (first)))

(defn part-2
  [input]
  (let [program (parse-program input)]
    (->> (for [noun (range 100)
               verb (range 100)]
           [noun verb])
         (reduce (fn [_ [noun verb]]
                   (let [result (-> (run-intcode-program (assoc program 1 noun 2 verb))
                                    (:memory)
                                    (first))]
                     (if (= result 19690720)
                       (reduced (+ (* 100 noun) verb))
                       nil)))
                 nil))))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 0.469875 msecs"
  ;=> 12490719

  (time (part-2 input))
  ;; "Elapsed time: 19.780708 msecs"
  ;=> 2003
  )
