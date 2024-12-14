(ns advent-of-code.year_2018.day_05)

(def input (slurp "src/advent_of_code/year_2018/inputs/day05.txt"))

(defn react-polymer
  [polymer]
  (loop [polymer polymer
         position 0]
    (if (= position (dec (count polymer)))
      polymer
      (let [this-char (nth polymer position)
            next-char (nth polymer (inc position))
            difference (- (int this-char) (int next-char))]
        (if (or (= difference 32) (= difference -32))
          (recur (str (subs polymer 0 position) (subs polymer (+ position 2)))
                 (max (dec position) 0))
          (recur polymer
                 (inc position)))))))

(defn part-1
  []
  (count (react-polymer input)))

(comment
  (time (part-1))
  ;; 9822
  ;; "Elapsed time: 143.624875 msecs"
  )

(defn clear-letter
  [polymer alphabet-index]
  (apply str (remove (fn [char]
                 (or (= (int char) (+ alphabet-index 65))
                     (= (int char) (+ alphabet-index 65 32))))
               polymer)))

(defn part-2
  []
  (reduce (fn [best-score alphabet-index]
            (min best-score (count (react-polymer (clear-letter input alphabet-index)))))
          (count input)
          (range 26)))


(comment
  (time (part-2))
  ;; 5726
  ;; "Elapsed time: 2616.870667 msecs"
  )
