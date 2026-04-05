(ns advent-of-code.year-2021.day-24
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2021/inputs/day24.txt"))
(def test-input "inp x\nmul x -1")
(def test-input-2 "inp z\ninp x\nmul z 3\neql z x")
(def test-input-3 "inp w\nadd z w\nmod z 2\ndiv w 2\nadd y w\nmod y 2\ndiv w 2\nadd x w\nmod x 2\ndiv w 2\nmod w 2")

(defn run-inp-instruction
  [{input :input values :values} var]
  {:values (assoc values var (first input))
   :input  (rest input)})

(defn var-or-number->number
  [values var-or-number]
  (if-let [var (re-matches #"[xyzw]" var-or-number)]
    (get values var)
    (read-string var-or-number)))

(defn run-add-instruction
  [state var var-or-number]
  (update state :values update var + (var-or-number->number (:values state) var-or-number)))

(defn run-mul-instruction
  [state var var-or-number]
  (update state :values update var * (var-or-number->number (:values state) var-or-number)))

(defn run-div-instruction
  [state var var-or-number]
  (update state :values update var quot (var-or-number->number (:values state) var-or-number)))

(defn run-mod-instruction
  [state var var-or-number]
  (update state :values update var mod (var-or-number->number (:values state) var-or-number)))

(defn run-eql-instruction
  [state var var-or-number]
  (update state :values update var (fn [value]
                                     (if (= value (var-or-number->number (:values state) var-or-number))
                                       1
                                       0))))

(defn run-alu-instruction
  [state instruction]
  (let [[instruction-type & arguments] (clojure.string/split instruction #"\s+")]
    (condp = instruction-type
      "inp" (apply run-inp-instruction state arguments)
      "add" (apply run-add-instruction state arguments)
      "mul" (apply run-mul-instruction state arguments)
      "div" (apply run-div-instruction state arguments)
      "div" (apply run-div-instruction state arguments)
      "mod" (apply run-mod-instruction state arguments)
      "eql" (apply run-eql-instruction state arguments))))

(defn run-alu-instructions
  {:test (fn []
           (is= (-> (run-alu-instructions (clojure.string/split-lines test-input) [5])
                    (get-in [:values "x"]))
                -5)
           (is= (-> (run-alu-instructions (clojure.string/split-lines test-input-2) [1 3])
                    (get-in [:values "z"]))
                1)
           (is= (-> (run-alu-instructions (clojure.string/split-lines test-input-2) [1 5])
                    (get-in [:values "z"]))
                0)
           (is= (-> (run-alu-instructions (clojure.string/split-lines test-input-3) [13])
                    (:values))
                {"w" 1 "x" 1 "y" 0 "z" 1})
           (is= (-> (run-alu-instructions (clojure.string/split-lines input) (repeat 14 1))
                    (get-in [:values "z"]))
                3600732497))}
  [instructions alu-input]
  (reduce run-alu-instruction
          {:input alu-input :values {"x" 0 "y" 0 "z" 0 "w" 0}}
          instructions))

;; There are no guarantees that this approach works in general, but it does here.
(defn get-next-numbers
  [instructions numbers current-value compare-fn]
  (->> (for [i (range (count numbers))
             j (range (count numbers))
             n1 (range 1 10)
             n2 (range 1 10)
             :when (not= i j)]
         [i n1 j n2])
       (reduce (fn [[_ best-value :as a] [i n1 j n2]]
                 (let [updated-numbers (-> numbers
                                           (assoc i n1)
                                           (assoc j n2))
                       value (-> (run-alu-instructions instructions updated-numbers)
                                 (get-in [:values "z"]))]
                   (cond
                     (< value best-value) [updated-numbers value]
                     (> value best-value) a
                     (compare-fn (compare updated-numbers numbers)) [updated-numbers value]
                     :else a)))
               [numbers current-value])))

(defn find-answer
  [input starting-numbers compare-fn]
  (let [instructions (clojure.string/split-lines input)]
    (loop [numbers starting-numbers
           value (-> (run-alu-instructions instructions numbers)
                     (get-in [:values "z"]))]
      (println (apply str numbers) value)
      (let [[next-numbers next-value] (get-next-numbers instructions numbers value compare-fn)]
        (if (= next-numbers numbers)
          (apply str numbers)
          (recur next-numbers next-value))))))

(defn part-1
  [input]
  (find-answer input (into [] (repeat 14 9)) pos?))

(defn part-2
  [input]
  (find-answer input (into [] (repeat 14 1)) neg?))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 14657.305416 msecs"
  ;=> "98998519596997"

  (time (part-2 input))
  ;; "Elapsed time: 31457.034334 msecs"
  ;=> "31521119151421"
  )
