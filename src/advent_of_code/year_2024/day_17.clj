(ns advent-of-code.year-2024.day-17
  (:require [ysera.test :refer [is= is is-not]]
            [clojure.math]))

(def input (slurp "src/advent_of_code/year_2024/inputs/day17.txt"))
(def test-input "Register A: 729\nRegister B: 0\nRegister C: 0\n\nProgram: 0,1,5,4,3,0\n")

(def test-state {:registers {:A 729
                             :B 0
                             :C 0}
                 :pointer   0
                 :output    []
                 :program   [0 1 5 4 3 0]})

(defn get-combo-operand
  [state operand]
  (condp = operand
    4 (get-in state [:registers :A])
    5 (get-in state [:registers :B])
    6 (get-in state [:registers :C])
    operand))

(defn adv
  {:test (fn []
           (is= (adv test-state 2)
                (assoc-in test-state [:registers :A] 182)))}
  [state operand]
  (update-in state [:registers :A] (fn [A]
                                     (long (/ A (clojure.math/pow 2
                                                                 (get-combo-operand state operand)))))))

(defn bxl
  {:test (fn []
           (is= (bxl {:registers {:B 5}} 4)
                {:registers {:B 1}}))}
  [state operand]
  (update-in state [:registers :B] (fn [B]
                                     (bit-xor B operand))))

(defn bst
  {:test (fn []
           (is= (bst {:registers {:B 5}} 2)
                {:registers {:B 2}})
           (is= (bst {:registers {:B 5}} 16)
                {:registers {:B 0}}))}
  [state operand]
  (assoc-in state [:registers :B] (mod (get-combo-operand state operand) 8)))

(defn jnz
  {:test (fn []
           (is= (jnz {:registers {:A 0}
                      :pointer   2}
                     10)
                {:registers {:A 0}
                 :pointer   4})
           (is= (jnz {:registers {:A 2}
                      :pointer   2}
                     10)
                {:registers {:A 2}
                 :pointer   10}))}
  [state operand]
  (if (zero? (get-in state [:registers :A]))
    (update state :pointer + 2)
    (assoc state :pointer operand)))

(defn bxc
  {:test (fn []
           (is= (bxc {:registers {:B 5
                                  :C 4}} "ignored")
                {:registers {:B 1
                             :C 4}}))}
  [state _]
  (update-in state [:registers :B] (fn [B]
                                     (bit-xor B (get-in state [:registers :C])))))

(defn out
  {:test (fn []
           (is= (out {:output [1 2]} 3)
                {:output [1 2 3]}))}
  [state operand]
  (update state :output conj (mod (get-combo-operand state operand) 8)))

(defn bdv
  {:test (fn []
           (is= (bdv {:registers {:A 729
                                  :B 0}} 2)
                {:registers {:A 729
                             :B 182}}))}
  [state operand]
  (assoc-in state [:registers :B] (long (/ (get-in state [:registers :A])
                                          (clojure.math/pow 2
                                                            (get-combo-operand state operand))))))

(defn cdv
  {:test (fn []
           (is= (cdv {:registers {:A 729
                                  :C 0}} 2)
                {:registers {:A 729
                             :C 182}}))}
  [state operand]
  (assoc-in state [:registers :C] (long (/ (get-in state [:registers :A])
                                          (clojure.math/pow 2
                                                            (get-combo-operand state operand))))))

(defn do-one-instruction
  [state]
  (let [operator (nth (:program state) (:pointer state) nil)
        operand (nth (:program state) (inc (:pointer state)) nil)]
    (if (nil? operator)
      state
      (case operator
        0 (-> (adv state operand)
              (update :pointer + 2))
        1 (-> (bxl state operand)
              (update :pointer + 2))
        2 (-> (bst state operand)
              (update :pointer + 2))
        3 (jnz state operand)
        4 (-> (bxc state operand)
              (update :pointer + 2))
        5 (-> (out state operand)
              (update :pointer + 2))
        6 (-> (bdv state operand)
              (update :pointer + 2))
        7 (-> (cdv state operand)
              (update :pointer + 2))))))

(defn part-1
  {:test (fn []
           (is= (part-1 test-state) "4,6,3,5,6,3,5,2,1,0"))}
  [state]
  (loop [state state]
    (let [next-state (do-one-instruction state)]
      (if (= next-state state)
        (clojure.string/join "," (:output state))
        (recur next-state)))))

(def test-state-2 {:registers {:A "ignored"
                               :B 0
                               :C 0}
                   :pointer   0
                   :output    []
                   :program   [0 3 5 4 3 0]})

(defn compare-programs
  {:test (fn []
           (is (compare-programs [1 2 3] [1 2]))
           (is-not (compare-programs [1 2 3] [2 2 3])))}
  [goal-program program]
  (every? true? (map = goal-program program)))

(defn part-2
  {:test (fn []
           (is= (part-2 test-state-2) 117440))}
  [state]
  (let [program (:program state)]
    (loop [A 0]
      ;(when (zero? (mod A 1000))
      ;  (println A))
      (let [result (loop [state (assoc-in state [:registers :A] A)]
                     (let [next-state (do-one-instruction state)]
                       (cond
                         (= next-state state) (:output state)
                         (not (compare-programs program (:output next-state))) :fail
                         :else (recur next-state))))]
        (if (= result program)
          A
          (recur (inc A)))))))

(comment
  (time (part-1 state))
  ;; "Elapsed time: 0.259833 msecs"
  ;=> "3,4,3,1,7,6,5,6,0"

  (part-1 (assoc-in state [:registers :A] 28147497671037))

  (time (part-2 state))
  ;;
  )
