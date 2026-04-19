(ns advent-of-code.year-2019.day-07
  (:require [ysera.test :refer [is= is is-not]]
            [advent-of-code.year-2019.intcode :refer [parse-program run-intcode-program]]
            [clojure.math.combinatorics :as combo]))

(def input (slurp "src/advent_of_code/year_2019/inputs/day07.txt"))
(def test-input "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0")
(def test-input-2 "3,23,3,24,1002,24,10,24,1002,23,-1,23,\n101,5,23,23,1,24,23,23,4,23,99,0,0")
(def test-input-3 "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,\n1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0")
(def test-input-4 "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,\n27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5\n")
(def test-input-5 "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,\n-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,\n53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10\n")

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 43210)
           (is= (part-1 test-input-2) 54321)
           (is= (part-1 test-input-3) 65210))}
  [input]
  (let [program (parse-program input)]
    (->> (combo/permutations (range 5))
         (reduce (fn [thruster-signal settings]
                   (max thruster-signal
                        (reduce (fn [input-output setting]
                                  (-> (run-intcode-program program [setting input-output])
                                      (:program-output)
                                      (first)))
                                0
                                settings)))
                 0))))

(defn run-programs-until-done
  [program settings]
  (loop [program-states (as-> settings $
                              (map vector $)
                              (into [] $)
                              (update $ 0 conj 0)
                              (map (fn [program-input]
                                     (run-intcode-program program program-input))
                                   $)
                              (into [] $))
         ;; Start on second program, since the first is the only one that has produced any output to use
         program-state-index 1]
    (let [program-state (nth program-states program-state-index)
          next-index (mod (inc program-state-index) (count settings))
          prev-index (mod (dec program-state-index) (count settings))]
      (if (= (:reason program-state) :halted)
        (if (= program-state-index (dec (count settings)))
          (first (:program-output program-state))
          (recur program-states next-index))
        (recur (-> program-states
                   (update prev-index assoc :program-output [])
                   (update program-state-index (fn [{memory :memory instruction-pointer :instruction-pointer}]
                                                 (run-intcode-program memory instruction-pointer (get-in program-states [prev-index :program-output]) [] 0))))
               next-index)))))

(defn part-2
    {:test (fn []
             (is= (part-2 test-input-4) 139629729)
             (is= (part-2 test-input-5) 18216))}
  [input]
  (let [program (parse-program input)]
    (->> (combo/permutations (range 5 10))
         (reduce (fn [thruster-signal settings]
                   (max thruster-signal
                        (run-programs-until-done program settings)))
                 0))))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 8.748666 msecs"
  ;=> 199988

  (time (part-2 input))
  ;; "Elapsed time: 26.516834 msecs"
  ;=> 17519904
  )
