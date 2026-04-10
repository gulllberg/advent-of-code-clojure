(ns advent-of-code.year-2019.day-02
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2019/inputs/day02.txt"))
(def test-input "")

(defn intcode-program
  {:test (fn []
           (is= (intcode-program [1 0 0 0 99]) 2)
           (is= (intcode-program [1 1 1 4 99 5 6 0 99]) 30))}
  [memory]
  (loop [memory memory
         instruction-pointer 0]
    (let [instruction (get memory instruction-pointer)]
      (condp = instruction
        99 (first memory)
        1 (let [first-input-address (get memory (inc instruction-pointer))
                second-input-address (get memory (+ 2 instruction-pointer))
                output-address (get memory (+ 3 instruction-pointer))]
            (recur (assoc memory output-address (+ (get memory first-input-address)
                                                   (get memory second-input-address)))
                   (+ 4 instruction-pointer)))
        2 (let [first-input-address (get memory (inc instruction-pointer))
                second-input-address (get memory (+ 2 instruction-pointer))
                output-address (get memory (+ 3 instruction-pointer))]
            (recur (assoc memory output-address (* (get memory first-input-address)
                                                   (get memory second-input-address)))
                   (+ 4 instruction-pointer)))
        (println "Invalid instruction" instruction)))))

(defn part-1
  [input]
  (as-> (re-seq #"\d+" input) $
        (map read-string $)
        (into [] $)
        (assoc $ 1 12 2 2)
        (intcode-program $)))

(defn part-2
  [input]
  (let [program (->> (re-seq #"\d+" input)
                     (map read-string)
                     (into []))]
    (->> (for [noun (range 100)
               verb (range 100)]
           [noun verb])
         (reduce (fn [_ [noun verb]]
                   (let [result (intcode-program (assoc program 1 noun 2 verb))]
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
