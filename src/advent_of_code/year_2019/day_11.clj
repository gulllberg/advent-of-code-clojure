(ns advent-of-code.year-2019.day-11
  (:require [ysera.test :refer [is= is is-not]]
            [advent-of-code.year-2019.intcode :refer [parse-program run-intcode-program]]
            [advent-of-code.grid :refer [turn-left turn-right]]))

(def input (slurp "src/advent_of_code/year_2019/inputs/day11.txt"))

(defn run-painting-robot
  [input starting-colour]
  (let [program (parse-program input)]
    (loop [painted {}
           position [0 0]
           direction [-1 0]
           program-result (run-intcode-program program [starting-colour])]
      (let [program-output (:program-output program-result)
            colour-to-paint (first program-output)
            turn-function (if (zero? (second program-output)) turn-left turn-right)
            new-direction (turn-function direction)
            new-position (map + position new-direction)
            new-painted (assoc painted position colour-to-paint)]
        (if (= (:reason program-result) :halted)
          new-painted
          (recur new-painted
                 new-position
                 new-direction
                 (run-intcode-program (:memory program-result)
                                      (:instruction-pointer program-result)
                                      [(get new-painted new-position 0)]
                                      []
                                      (:relative-base program-result))))))))

(defn part-1
  [input]
  (-> (run-painting-robot input 0)
      (keys)
      (count)))

(defn print-paint
  [painted]
  (let [positions (keys painted)
        row-values (map first positions)
        column-values (map second positions)
        min-row (apply min row-values)
        max-row (apply max row-values)
        min-column (apply min column-values)
        max-column (apply max column-values)]
    (doseq [i (range min-row (inc max-row))]
      (let [row-str (->> (range min-column (inc max-column))
                         (map (fn [j]
                                (if (= 1 (get painted [i j]))
                                  "#"
                                  " ")))
                         (apply str))]
        (println row-str)))))


(defn part-2
  [input]
  (-> (run-painting-robot input 1)
      (print-paint)))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 124.064334 msecs"
  ;=> 1967

  (time (part-2 input))
  ; #  # ###  #  # ####  ##  #### ###  #  #
  ; # #  #  # #  # #    #  #    # #  # # #
  ; ##   ###  #  # ###  #      #  ###  ##
  ; # #  #  # #  # #    # ##  #   #  # # #
  ; # #  #  # #  # #    #  # #    #  # # #
  ; #  # ###   ##  ####  ### #### ###  #  #
  ;; "Elapsed time: 18.74275 msecs"
  ;=> nil
  )
