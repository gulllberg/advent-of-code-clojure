(ns advent-of-code.year-2015.day-12
  (:require [ysera.test :refer [is= is is-not]]
            [clojure.data.json :as json]))

(def input (slurp "src/advent_of_code/year_2015/inputs/day12.txt"))

(defn part-1
  {:test (fn []
           (is= (part-1 "{\"a\":{\"b\":4},\"c\":-1}") 3))}
  [input]
  (->> (re-seq #"-?\d+" input)
       (map read-string)
       (reduce +)))

(defn recursively-sum-json
  [value]
  (cond
    (number? value) value
    (string? value) 0
    (vector? value)
    (reduce + (map recursively-sum-json value))
    :else-object
    (let [values (vals value)]
      (if (some #{"red"} values)
        0
        (reduce + (map recursively-sum-json values))))))

(defn part-2
  {:test (fn []
           (is= (part-2 "[1,{\"c\":\"red\",\"b\":2},3]") 4)
           (is= (part-2 "{\"d\":\"red\",\"e\":[1,2,3,4],\"f\":5}") 0)
           (is= (part-2 "[1,\"red\",5]") 6))}
  [input]
  (recursively-sum-json (json/read-str input)))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 4.6125 msecs"
  ;=> 119433

  (time (part-2 input))
  ;; "Elapsed time: 5.583584 msecs"
  ;=> 68466
  )
