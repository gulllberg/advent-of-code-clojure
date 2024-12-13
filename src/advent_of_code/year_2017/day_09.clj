(ns advent-of-code.year-2017.day-09
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2017/inputs/day09.txt"))

(defn problem-9a
  {:test (fn []
           (is= (problem-9a "{}") 1)
           (is= (problem-9a "{{{}}}") 6)
           (is= (problem-9a "{{},{}}") 5)
           (is= (problem-9a "{{{},{},{{}}}}") 16)
           (is= (problem-9a "{<a>,<a>,<a>,<a>}") 1)
           (is= (problem-9a "{{<ab>},{<ab>},{<ab>},{<ab>}}") 9)
           (is= (problem-9a "{{<!!>},{<!!>},{<!!>},{<!!>}}") 9)
           (is= (problem-9a "{{<a!>},{<a!>},{<a!>},{<ab>}}") 3))}
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

(defn problem-9b
  {:test (fn []
           (is= (problem-9b "{<>}") 0)
           (is= (problem-9b "{<random characters>}") 17)
           (is= (problem-9b "{<<<<>}") 3)
           (is= (problem-9b "{<{!>}>}") 2)
           (is= (problem-9b "{<!!>}") 0)
           (is= (problem-9b "{<!!!>>}") 0)
           (is= (problem-9b "{<{o\"i!a,<{i<a>}") 10))}
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
  (problem-9a input)
  ;; 14212
  (problem-9b input)
  ;; 6569
  )
