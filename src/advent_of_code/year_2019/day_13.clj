(ns advent-of-code.year-2019.day-13
  (:require [ysera.test :refer [is= is is-not]]
            [advent-of-code.year-2019.intcode :refer [parse-program run-intcode-program]]))

(def input (slurp "src/advent_of_code/year_2019/inputs/day13.txt"))

(defn part-1
  [input]
  (->> (parse-program input)
       (run-intcode-program)
       (:program-output)
       (partition 3)
       (map last)
       (filter #{2})
       (count)))

(defn get-draw-state
  [program-output]
  (->> (partition 3 program-output)
       (reduce (fn [a [x y id]]
                 (if-let [c ({0 " "
                              1 "#"
                              2 "X"
                              3 "="
                              4 "O"} id)]
                   (assoc a [x y] c)
                   a))
               {})))

(defn draw-row
  [draw-state row]
  (let [draw-row-state (->> draw-state
                            (reduce-kv (fn [a [x y] c]
                                         (if (= y row)
                                           (assoc a x c)
                                           a))
                                       {}))
        ks (keys draw-row-state)
        from (apply min ks)
        to (apply max ks)
        draw-str (->> (range from (inc to))
                      (map draw-row-state)
                      (apply str))]
    (println draw-str)))

(defn draw-display
  [program-output]
  (let [draw-state (get-draw-state program-output)
        row-ks (->> (keys draw-state)
                    (map second))
        from (apply min row-ks)
        to (apply max row-ks)]
    (doseq [row (range from (inc to))]
      (draw-row draw-state row))))

(defn get-score
  [program-output]
  (->> program-output
       (partition 3)
       (filter (fn [[x y _]]
                 (and (= x -1) (zero? y))))
       (last)
       (last)))

(defonce program-input-atom (atom (read-string (slurp "src/advent_of_code/year_2019/day_13-program-input.txt"))))

(defn part-2
  [input]
  (let [program-input (deref program-input-atom)]
    (as-> (parse-program input) $
          (assoc $ 0 2)
          (run-intcode-program $ program-input)
          (:program-output $))))

(defn move-left!
  [input]
  (swap! program-input-atom conj -1)
  (draw-display (part-2 input)))

(defn move-right!
  [input]
  (swap! program-input-atom conj 1)
  (draw-display (part-2 input)))

(defn stay!
  [input]
  (swap! program-input-atom conj 0)
  (draw-display (part-2 input)))

(defn undo!
  [input]
  (swap! program-input-atom (fn [inp]
                              (if (= (count inp) 1)
                                inp
                                (into [] (drop-last inp)))))
  (draw-display (part-2 input)))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 33.248459 msecs"
  ;=> 298

  (move-left! input)
  (move-right! input)
  (stay! input)
  (undo! input)
  (count (deref program-input-atom))
  (get-score (part-2 input))

  (let [program-input (deref program-input-atom)]
    (as-> (parse-program input) $
          (assoc $ 0 2)
          (run-intcode-program $ program-input)))

  ;; This is not quite a finished game
  (spit "src/advent_of_code/year_2019/day_13-program-input.txt" (deref program-input-atom))

  ;(time (part-2 input))
  ;; 13956 from playing the game, see https://github.com/gulllberg/advent-of-code-visualisations
  )
