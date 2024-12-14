(ns advent-of-code.year-2018.day_01)

(def input (slurp "src/advent_of_code/year_2018/inputs/day01.txt"))

;; TODO: sub not necessary
(defn solve-a []
  (reduce (fn [result instruction]
            (let [operator (resolve (read-string (subs instruction 0 1)))
                  number (read-string (subs instruction 1))]
              (operator result number)))
          0
          (clojure.string/split-lines input)))

(comment
  (solve-a)
  ;; 406
  )

(defn solve-b []
  (reduce (fn [[last-frequency previous-frequencies] instruction]
            (let [next-frequency (+ last-frequency (read-string instruction))]
              (if (contains? previous-frequencies next-frequency)
                (reduced next-frequency)
                [next-frequency (conj previous-frequencies next-frequency)])))
          [0 #{}]
          (cycle (clojure.string/split-lines input))))

(comment
  (solve-b)
  ;; 312
  )
