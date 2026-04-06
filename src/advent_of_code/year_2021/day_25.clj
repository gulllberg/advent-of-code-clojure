(ns advent-of-code.year-2021.day-25
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2021/inputs/day25.txt"))
(def test-input "v...>>.vv>\n.vv>>.vv..\n>>.>v>...v\n>>v>>.>.v.\nv>v.vv.v..\n>.>>..v...\n.vv..>.>v.\nv.v..>>v.v\n....v..v.>")

(defn parse-input
  [input]
  (let [lines (into [] (clojure.string/split-lines input))]
    (reduce-kv (fn [a i line]
                 (reduce-kv (fn [a j c]
                              (condp = c
                                \> (update a :east conj [i j])
                                \v (update a :south conj [i j])
                                a))
                            a
                            (into [] line)))
               {:east #{} :south #{} :rows (count lines) :cols (count (into [] (first lines)))}
               lines)))

(defn get-next-position-east
  [state [i j]]
  [i (mod (inc j) (:cols state))])

(defn get-next-position-south
  [state [i j]]
  [(mod (inc i) (:rows state)) j])

(defn move-east-herd
  [state]
  (reduce (fn [new-state position]
            (let [next-position (get-next-position-east state position)]
              (if (or (contains? (:east state) next-position)
                      (contains? (:south state) next-position))
                (update new-state :east conj position)
                (update new-state :east conj next-position))))
          (assoc state :east #{})
          (:east state)))

(defn move-south-herd
  [state]
  (reduce (fn [new-state position]
            (let [next-position (get-next-position-south state position)]
              (if (or (contains? (:east state) next-position)
                      (contains? (:south state) next-position))
                (update new-state :south conj position)
                (update new-state :south conj next-position))))
          (assoc state :south #{})
          (:south state)))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 58))}
  [input]
  (loop [state (parse-input input)
         steps 0]
    (let [new-state (-> state
                        (move-east-herd)
                        (move-south-herd))]
      (if (= state new-state)
        (inc steps)
        (recur new-state (inc steps))))))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 1837.913792 msecs"
  ;=> 568
  )
