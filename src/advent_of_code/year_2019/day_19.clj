(ns advent-of-code.year-2019.day-19
  (:require [ysera.test :refer [is= is is-not]]
            [advent-of-code.year-2019.intcode :refer [parse-program run-intcode-program]]))

(def input (slurp "src/advent_of_code/year_2019/inputs/day19.txt"))

(defn part-1
  [input]
  (let [program (parse-program input)]
    (->> (for [X (range 50)
               Y (range 50)]
           [X Y])
         (map (fn [program-input]
                (-> (run-intcode-program program program-input)
                    (:program-output)
                    (first))))
         (remove zero?)
         (count))))

(defn in-tractor-beam?
  [program program-input]
  (-> (run-intcode-program program program-input)
      (:program-output)
      (first)
      (zero?)
      (not)))

(defn get-left
  {:test (fn []
           (is= (get-left (parse-program input) 1 0) 2)
           (is= (get-left (parse-program input) 9 15) 16))}
  [program row previous-left]
  (if (in-tractor-beam? program [previous-left row])
    (throw (Exception. "this does not happen"))
    (loop [X (inc previous-left)]
      (if (in-tractor-beam? program [X row])
        X
        (recur (inc X))))))

(defn get-right
  {:test (fn []
           (is= (get-right (parse-program input) 1 0) 2)
           (is= (get-right (parse-program input) 9 17) 19)
           (is= (get-right (parse-program input) 5 8) 10))}
  [program row previous-right]
  (if (in-tractor-beam? program [previous-right row])
    (loop [X (inc previous-right)]
      (if (in-tractor-beam? program [X row])
        (recur (inc X))
        (dec X)))
    ;; The case below starts to the left of the entire beam
    (loop [X (inc previous-right)
             found-beam false]
        (cond
          (in-tractor-beam? program [X row])
          (recur (inc X) true)

          found-beam
          (dec X)

          :else
          (recur (inc X) false)))))

(defn get-worst-left-right
  [rows]
  (->> (take-last 100 rows)
       (reduce (fn [[worst-left worst-right] [left right]]
                 [(max worst-left left) (min worst-right right)])
               [##-Inf ##Inf])))

(defn finished?
  [rows]
  (let [[left right] (get-worst-left-right rows)
        width (inc (- right left))]
    (>= width 100)))

(defn calculate-answer
  [X Y]
  (println "for answer" X Y)
  (+ (* 10000 X) Y))

(defn get-answer
  [rows]
  (let [Y (- (count rows) 100)
        [X _] (get-worst-left-right rows)]
    (calculate-answer X Y)))

(defn part-2
  [input]
  (let [program (parse-program input)]
    ;; row = Y coordinate, left/right = X coordinate
    (loop [rows [[0 0]]]
      (let [row (count rows)
            previous-left (first (last rows))
            previous-right (last (last rows))]
        (if (finished? rows)
          (get-answer rows)
          (recur (conj rows [(get-left program row previous-left)
                             (get-right program row previous-right)])))))))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 533.055667 msecs"
  ;=> 129

  (time (part-2 input))
  ;; "Elapsed time: 1140.254125 msecs"
  ;=> 14040699
  )
