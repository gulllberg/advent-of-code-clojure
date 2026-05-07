(ns advent-of-code.year-2019.day-15
  (:require [ysera.test :refer [is= is is-not]]
            [advent-of-code.year-2019.intcode :refer [parse-program run-intcode-program]]
            [advent-of-code.grid :refer [directions-without-diagonals]]))

(def input (slurp "src/advent_of_code/year_2019/inputs/day15.txt"))

(def direction->command {[-1 0] 1
                         [1 0]  2
                         [0 -1] 3
                         [0 1]  4})

(defn found-oxygen?
  [boundary]
  (some (fn [{program-state :program-state moves :moves}]
          (when (= (get-in program-state [:program-output 0]) 2)
            moves))
        boundary))

(defn move-robot
  [input stop-at-oxygen initial-moves]
  (let [initial-program-state (-> (parse-program input)
                                  (run-intcode-program (map direction->command initial-moves)))
        initial-position (apply mapv + [0 0] initial-moves)]
    (loop [visited #{initial-position}
           boundary [{:position initial-position :moves [] :program-state initial-program-state}]
           steps 0]
      (if (empty? boundary)
        (dec steps)
        (let [[visited boundary] (reduce (fn [[visited boundary] {position :position moves :moves program-state :program-state}]
                                           (reduce (fn [[visited boundary] direction]
                                                     (let [new-position (mapv + position direction)]
                                                       (if (contains? visited new-position)
                                                         [visited boundary]
                                                         (let [program-state (run-intcode-program (:memory program-state)
                                                                                                  (:instruction-pointer program-state)
                                                                                                  [(direction->command direction)]
                                                                                                  []
                                                                                                  (:relative-base program-state))
                                                               could-move (not= (get-in program-state [:program-output 0]) 0)]
                                                           [(conj visited new-position)
                                                            (if could-move
                                                              (conj boundary {:position new-position :moves (conj moves direction) :program-state program-state})
                                                              boundary)]))))
                                                   [visited boundary]
                                                   directions-without-diagonals))
                                         [visited []]
                                         boundary)
              steps (inc steps)]
          (if-let [moves (and stop-at-oxygen
                              (found-oxygen? boundary))]
            [steps moves]
            (recur visited boundary steps)))))))

(defn part-1
  [input]
  (-> (move-robot input true [])
      (first)))

(defn part-2
  [input]
  (as-> (move-robot input true []) $
        (second $)
        (move-robot input false $)))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 17.452375 msecs"
  ;=> 266

  (time (part-2 input))
  ;; "Elapsed time: 53.639541 msecs"
  ;=> 274
  )
