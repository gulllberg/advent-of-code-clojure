(ns advent-of-code.year-2020.day-15)

(def input (->> (slurp "src/advent_of_code/year_2020/inputs/day15.txt")
                (re-seq #"\d+")
                (map read-string)
                (into [])))

(defn process-input
  [input]
  (reduce (fn [s i]
            (assoc s (nth input i) i))
          {}
          ; Don't add last number yet
          (range (dec (count input)))))

(defn solve-for-number
  [number]
  (loop [i 5
         n 6
         s (process-input input)]
    (if (= i (dec number))
      n
      (recur (inc i)
             (if-let [last-appearance (get s n)]
               (- i last-appearance)
               0)
             (assoc s n i)))))

(defn part-1
  []
  (solve-for-number 2020))

(comment
  (time (part-1))
  ; 441
  ; "Elapsed time: 1.533917 msecs"
  )

(defn part-2
  []
  (solve-for-number 30000000))

(comment
  (time (part-2))
  ; 10613991
  ; "Elapsed time: 16521.881516 msecs"
  )
