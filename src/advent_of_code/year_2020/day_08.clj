(ns advent-of-code.year-2020.day-08)

(def input (slurp "src/advent_of_code/year_2020/inputs/day08.txt"))

(defn parse-line
  [line]
  (let [parts (clojure.string/split line #" ")]
    [(first parts) (read-string (second parts))]))

(defn part-1
  []
  (let [lines (clojure.string/split-lines input)]
    (loop [acc 0
           line-number 0
           visited #{}]
      (if (contains? visited line-number)
        acc
        (let [[instruction number] (parse-line (nth lines line-number))]
          (recur (if (= instruction "acc") (+ acc number) acc)
                 (if (= instruction "jmp") (+ line-number number) (inc line-number))
                 (conj visited line-number)))))))

(comment
  (time (part-1))
  ; 2080
  ; "Elapsed time: 1.030541 msecs
  )

(defn does-it-terminate?
  [lines line-number-to-modify]
  (loop [acc 0
         line-number 0
         visited #{}]
    (cond
      (= line-number (count lines)) acc
      (contains? visited line-number) false
      :else (let [[instruction number] (parse-line (nth lines line-number))
                  instruction (cond
                                (and (= line-number line-number-to-modify) (= instruction "nop")) "jmp"
                                (and (= line-number line-number-to-modify) (= instruction "jmp")) "nop"
                                :else instruction)]
              (recur (if (= instruction "acc") (+ acc number) acc)
                     (if (= instruction "jmp") (+ line-number number) (inc line-number))
                     (conj visited line-number))))))

(defn part-2
  []
  (let [lines (clojure.string/split-lines input)]
    (loop [line-number-to-modify 0]
      (if-let [acc (does-it-terminate? lines line-number-to-modify)]
        acc
        (recur (inc line-number-to-modify))))))

(comment
  (time (part-2))
  ; 2477
  ; "Elapsed time: 69.985458 msecs"
  )
