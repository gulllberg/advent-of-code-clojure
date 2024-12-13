(ns advent-of-code.year-2017.day-03)

(def input (read-string (slurp "src/advent_of_code/year_2017/inputs/day03.txt")))

(defn get-level
  [input]
  (some (fn [level]
          (let [side (- (* 2 level) 1)]
            (when (>= (* side side) input)
              level)))
        (range)))

(defn get-side-steps
  [input]
  (let [level (get-level input)
        level-length (- (* 2 level) 1)
        previous-level-length (- (* 2 (dec level)) 1)
        step-number (- input (* previous-level-length previous-level-length))
        mod-step-number (mod step-number (dec level-length))
        max-side-distance (/ (dec level-length) 2)]
    (Math/abs (- max-side-distance mod-step-number))))

(defn part-1
  [input]
  (+ (dec (get-level input))
     (get-side-steps input)))

(defn part-2
  [input]
  (loop [[x y] [0 1]
         grid {"0+0" 1}]
    (let [north (get grid (str x "+" (inc y)))
          north-west (get grid (str (dec x) "+" (inc y)))
          west (get grid (str (dec x) "+" y))
          south-west (get grid (str (dec x) "+" (dec y)))
          south (get grid (str x "+" (dec y)))
          south-east (get grid (str (inc x) "+" (dec y)))
          east (get grid (str (inc x) "+" y))
          north-east (get grid (str (inc x) "+" (inc y)))
          sum (->> [north north-west west south-west south south-east east north-east]
                   (map (fn [v] (or v 0)))
                   (apply +))
          next-direction (cond
                           (and (not (nil? north))
                                (not (nil? west)))
                           [1 0]

                           (not (nil? west))
                           [0 1]

                           (not (nil? south))
                           [-1 0]

                           (not (nil? east))
                           [0 -1]

                           (not (nil? north))
                           [1 0]

                           :else
                           [1 0])]
      (if (> sum input)
        sum
        (recur (mapv + [x y] next-direction)
               (assoc grid
                 (str x "+" y) sum
                 (str x "+" (inc y)) north
                 (str (dec x) "+" (inc y)) north-west
                 (str (dec x) "+" y) west
                 (str (dec x) "+" (dec y)) south-west
                 (str x "+" (dec y)) south
                 (str (inc x) "+" (dec y)) south-east
                 (str (inc x) "+" y) east
                 (str (inc x) "+" (inc y)) north-east))))))
(comment

  (time (part-1 input))
  ;; 552
  ;; "Elapsed time: 4.162667 msecs"

  (time (part-2 input))
  ;; 330785
  ;; "Elapsed time: 1.600041 msecs"
  )
