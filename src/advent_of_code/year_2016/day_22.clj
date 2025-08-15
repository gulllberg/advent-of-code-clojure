(ns advent-of-code.year-2016.day-22
  (:require [ysera.test :refer [is= is is-not]]
            [advent-of-code.grid :as g]
            [clojure.data.priority-map :refer [priority-map]]))

(def input (slurp "src/advent_of_code/year_2016/inputs/day22.txt"))

(defn parse-line
  [line]
  (->> (re-find #"/dev/grid/node-x(\d+)-y(\d+)\s+(\d+)T\s+(\d+)T\s+\d+T\s+\d+%" line)
       (rest)
       (map read-string)))

(defn parse-grid
  [input]
  (->> (clojure.string/split-lines input)
       (drop 2)
       (reduce (fn [a line]
                 (let [[x y size used] (parse-line line)]
                   (assoc a [x y] {:size size :used used})))
               {})))

(defn count-viable-pairs
  [grid]
  (reduce-kv (fn [a k-A v-A]
               (if (zero? (:used v-A))
                 a
                 (reduce (fn [a v-B]
                           (if (> (:used v-A) (- (:size v-B) (:used v-B)))
                             a
                             (inc a)))
                         a
                         (vals (dissoc grid k-A)))))
             0
             grid))

(defn part-1
  [input]
  (-> input
      (parse-grid)
      (count-viable-pairs)))

;; Observations on data for part 2:
;; 1. One node is empty (the one we can move data to)
;; 2. Some nodes are large (about 500T) and almost full. Can never move this data somewhere else.
;; 3. The other nodes are all small enough that we can move the data onto the empty node.
;; This creates a Slide Puzzle (15-spel) with some inaccessible parts of the grid.
(defn create-state
  [grid]
  (reduce-kv (fn [state pos {used :used}]
               ;; Check if node can store data -> add to grid
               (let [state (if (> used 200)
                             state
                             (update state :grid conj pos))
                     ;; Check if empty position
                     state (if (zero? used)
                             (assoc state :empty-position pos)
                             state)
                     ;; Check if target position (candidate) - top right corner node
                     [x y] pos
                     state (if (and (zero? y)
                                    (> x (get-in state [:data-position 0])))
                             (assoc state :data-position pos)
                             state)]
                 state))
             {:grid           #{}
              :goal           [0 0]
              :data-position  [0 0]
              :empty-position nil}
             grid))

(defn move-data-one-step
  {:test (fn []
           (let [state
                 (-> input
                     (parse-grid)
                     (create-state))]
             (is= (move-data-one-step state [28 0]) 48)
             (is= (move-data-one-step (assoc state :data-position [28 0] :empty-position [29 0]) [27 0]) 5)))}
  [state to]
  (loop [visited #{(:empty-position state)}
         boundary #{(:empty-position state)}
         steps 0]
    (if (contains? boundary to)
      ;; After empty space is next to data, one more step to move data onto empty node
      (inc steps)
      (let [boundary (reduce (fn [a p]
                               (reduce (fn [a n]
                                         (if (and (contains? (:grid state) n)
                                                  (not (contains? visited n))
                                                  ;; Cannot move through the data (would move it to wrong square)
                                                  (not= (:data-position state) n))
                                           (conj a n)
                                           a))
                                       a
                                       (g/get-neighbours p)))
                             #{}
                             boundary)]
        (recur (clojure.set/union visited boundary) boundary (inc steps))))))

(defn move-data-to-end
  [state]
  (let [grid (:grid state)
        goal (:goal state)
        start-pos {:empty-position (:empty-position state)
                   :data-position  (:data-position state)}]
    (loop [visited #{start-pos}
           queue (priority-map start-pos 0)]
      (let [[{empty-position :empty-position data-position :data-position} steps] (peek queue)]
        (if (= data-position goal)
          steps
          (recur (conj visited {:empty-position empty-position :data-position data-position})
                 (reduce (fn [queue n]
                           (if-not (and (contains? grid n)
                                        (not (contains? visited n))
                                        )
                             queue
                             (let [extra-steps (move-data-one-step {:grid           grid
                                                                    :goal           goal
                                                                    :empty-position empty-position
                                                                    :data-position  data-position}
                                                                   n)
                                   ;; After moving data one step, the empty space will be where data used to be
                                   new-k {:empty-position data-position :data-position n}]
                               ;; Update with min way to get to position
                               (assoc queue new-k (min (+ steps extra-steps) (get queue new-k ##Inf))))))
                         (pop queue)
                         (g/get-neighbours data-position))))))))

(defn part-2
  [input]
  (-> input
      (parse-grid)
      (create-state)
      (move-data-to-end)))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 47.828583 msecs"
  ;=> 937

  (time (part-2 input))
  ;; "Elapsed time: 4156.927709 msecs"
  ;=> 188
  )
