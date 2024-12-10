(ns advent-of-code.year-2020.day-25)

(def card-input (clojure.string/trim-newline (slurp "src/advent_of_code/year_2020/inputs/day25card.txt")))
(def door-input (clojure.string/trim-newline (slurp "src/advent_of_code/year_2020/inputs/day25door.txt")))

(defn find-loop-size
  [subject-number target]
  (loop [i 1
         number subject-number]
    (let [new-number (rem (* number subject-number) 20201227)]
      (if (= new-number target)
        (inc i)
        (recur (inc i) new-number)))))

(defn perform-loop
  [subject-number loop-size]
  (loop [i 1
         number subject-number]
    (if (= i loop-size)
      number
      (recur (inc i) (rem (* number subject-number) 20201227)))))

(defn solve-a
  []
  (let [door-loop-size (find-loop-size 7 (read-string card-input))]
    (perform-loop (read-string door-input) door-loop-size)))

(comment
  (solve-a)
  ; 19414467
  )
