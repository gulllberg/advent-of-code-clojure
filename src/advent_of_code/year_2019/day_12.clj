(ns advent-of-code.year-2019.day-12
  (:require [ysera.test :refer [is= is is-not]]
            [advent-of-code.grid :refer [manhattan-distance-3d]]
            [clojure.math :refer [signum round]]
            [advent-of-code.maths :refer [lcm]]))

(def input (slurp "src/advent_of_code/year_2019/inputs/day12.txt"))
(def test-input "<x=-1, y=0, z=2>\n<x=2, y=-10, z=-7>\n<x=4, y=-8, z=8>\n<x=3, y=5, z=-1>")
(def test-input-2 "<x=-8, y=-10, z=0>\n<x=5, y=5, z=10>\n<x=2, y=-7, z=3>\n<x=9, y=-8, z=-3>")

(defn parse-input
  [input]
  (->> (clojure.string/split-lines input)
       (map (fn [line]
              (map read-string (re-seq #"-?\d+" line))))))

(defn get-new-velocity
  {:test (fn []
           (is= (get-new-velocity [[1 2 3] [5 0 3]] [1 2 3] [-2 2 0]) [-1 1 0])
           (is= (get-new-velocity [[1] [5]] [1] [-2]) [-1]))}
  [positions position velocity]
  (reduce (fn [velocity other-position]
            (->> (map - other-position position)
                 (map signum)
                 (map round)
                 (map + velocity)))
          velocity
          positions))

(defn get-energy
  {:test (fn []
           (is= (get-energy {:position [2 1 3] :velocity [3 2 1]})
                36))}
  [{position :position velocity :velocity}]
  (* (manhattan-distance-3d position [0 0 0])
     (manhattan-distance-3d velocity [0 0 0])))

(defn get-next-state
  [state]
  (let [positions (map :position state)]
    (->> state
         (map (fn [{position :position velocity :velocity}]
                {:position position :velocity (get-new-velocity positions position velocity)}))
         (map (fn [{position :position velocity :velocity}]
                {:position (map + position velocity) :velocity velocity})))))

(defn simulate-n-steps
  [input n]
  (let [positions (parse-input input)
        state (map (fn [position]
                     {:position position :velocity [0 0 0]})
                   positions)]
    (->> (range n)
         (reduce (fn [state _]
                   (get-next-state state))
                 state))))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input 10) 179)
           (is= (part-1 test-input-2 100) 1940))}
  ([input]
   (part-1 input 1000))
  ([input steps]
   (->> (simulate-n-steps input steps)
        (map get-energy)
        (reduce +))))

(defn find-cycle
  [initial-state]
  (loop [state initial-state
         steps 0]
    (let [state (get-next-state state)
          steps (inc steps)]
      (if (= initial-state state)
        steps
        (recur state steps)))))

(defn extract-dimension
  [state index]
  (->> state
       (map (fn [{position :position velocity :velocity}]
              {:position [(nth position index)] :velocity [(nth velocity index)]}))))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input) 2772)
           (is= (part-2 test-input-2) 4686774924))}
  [input]
  (let [positions (parse-input input)
        state (map (fn [position]
                     {:position position :velocity [0 0 0]})
                   positions)
        cycles (->> (range 3)
                    (map (fn [index] (extract-dimension state index)))
                    (map find-cycle))]
    (reduce lcm cycles)))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 23.221292 msecs"
  ;=> 13399

  (time (part-2 input))
  ;; "Elapsed time: 3736.024458 msecs"
  ;=> 312992287193064
  )
