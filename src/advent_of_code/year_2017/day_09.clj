(ns advent-of-code.year-2017.day-09
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2017/inputs/day09.txt"))

(defn part-1
  {:test (fn []
           (is= (part-1 "{}") 1)
           (is= (part-1 "{{{}}}") 6)
           (is= (part-1 "{{},{}}") 5)
           (is= (part-1 "{{{},{},{{}}}}") 16)
           (is= (part-1 "{<a>,<a>,<a>,<a>}") 1)
           (is= (part-1 "{{<ab>},{<ab>},{<ab>},{<ab>}}") 9)
           (is= (part-1 "{{<!!>},{<!!>},{<!!>},{<!!>}}") 9)
           (is= (part-1 "{{<a!>},{<a!>},{<a!>},{<ab>}}") 3))}
  [input]
  (let [slimmed-input (clojure.string/replace input #"!." "")]
    (->> slimmed-input
         (reduce (fn [[sum group-depth in-garbage?] char]
                   (let [character (str char)]
                     (cond
                       (and in-garbage? (not= character ">"))
                       [sum group-depth true]

                       (and in-garbage? (= character ">"))
                       [sum group-depth false]

                       (and (not in-garbage?) (= character "<"))
                       [sum group-depth true]

                       (and (not in-garbage?) (= character "{"))
                       [sum (inc group-depth) false]

                       (and (not in-garbage?) (= character "}"))
                       [(+ sum group-depth) (dec group-depth) false]

                       :else
                       [sum group-depth false])))
                 [0 0 false]
                 )
         (first))))

(defn part-2
  {:test (fn []
           (is= (part-2 "{<>}") 0)
           (is= (part-2 "{<random characters>}") 17)
           (is= (part-2 "{<<<<>}") 3)
           (is= (part-2 "{<{!>}>}") 2)
           (is= (part-2 "{<!!>}") 0)
           (is= (part-2 "{<!!!>>}") 0)
           (is= (part-2 "{<{o\"i!a,<{i<a>}") 10))}
  [input]
  (let [slimmed-input (clojure.string/replace input #"!." "")]
    (->> slimmed-input
         (reduce (fn [[sum in-garbage?] char]
                   (let [character (str char)]
                     (cond
                       (and in-garbage? (not= character ">"))
                       [(inc sum) true]

                       (and in-garbage? (= character ">"))
                       [sum false]

                       (and (not in-garbage?) (= character "<"))
                       [sum true]

                       :else
                       [sum false])))
                 [0 false]
                 )
         (first))))

(comment
  (time (part-1 input))
  ;; 14212
  ;; "Elapsed time: 7.409 msecs"

  (time (part-2 input))
  ;; 6569
  ;; "Elapsed time: 4.688084 msecs"
  )
