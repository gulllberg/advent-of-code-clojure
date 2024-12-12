(ns advent-of-code.year-2021.day-03
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2021/inputs/day03.txt"))
(def test-input ["00100"
                  "11110"
                  "10110"
                  "10111"
                  "10101"
                  "01111"
                  "00111"
                  "11100"
                  "10000"
                  "11001"
                  "00010"
                  "01010"])

(defn sum-binary-strings
  {:test (fn []
           (is= (sum-binary-strings test-input)
                [7 5 8 7 5]))}
  [binary-strings]
  (->> binary-strings
       (map (fn [s] (map read-string (clojure.string/split s #""))))
       (apply mapv +)))

;; https://stackoverflow.com/questions/5057047/how-to-do-exponentiation-in-clojure
(defn exp [x n]
  (reduce * (repeat n x)))

(defn binary-list->number
  {:test (fn []
           (is= (binary-list->number [1 0 1 1 0]) 22)
           (is= (binary-list->number [0 1 0 0 1]) 9))}
  [binary-list]
  (reduce-kv (fn [a i v]
               (+ a (* v (exp 2 i))))
             0
             (vec (reverse binary-list))))

(defn solve-a
  []
  (let [most-common (sum-binary-strings (clojure.string/split-lines input))
        [gamma-list epsilon-list] (reduce (fn [[g e] sum-in-position]
                                  (let [g-this (if (> sum-in-position 500) 1 0)
                                        e-this (- 1 g-this)]
                                    [(conj g g-this) (conj e e-this)]))
                                [[] []]
                                most-common)]
    (* (binary-list->number gamma-list) (binary-list->number epsilon-list))))

(comment
  (solve-a)
  ; 845186
  )

(defn filter-number
  {:test (fn []
           (is= (filter-number test-input >=) "10111")
           (is= (filter-number test-input <) "01010"))}
  ([binary-strings criterion position]
   (if (= (count binary-strings) 1)
     (first binary-strings)
     (let [number-to-be (if (criterion (* 2 (nth (sum-binary-strings binary-strings) position)) (count binary-strings))
                                1 0)]
       (filter-number (filter (fn [s]
                                (= number-to-be (read-string (subs s position (inc position)))))
                              binary-strings)
                      criterion
                      (inc position)))))
  ([binary-strings criterion]
   (filter-number binary-strings criterion 0)))

(defn solve-b
  []
  (let [binary-strings (clojure.string/split-lines input)
        oxygen-generator-rating-string (filter-number binary-strings >=)
        CO2-scrubber-rating-string (filter-number binary-strings <)]
    (* (binary-list->number (map read-string (clojure.string/split oxygen-generator-rating-string #"")))
       (binary-list->number (map read-string (clojure.string/split CO2-scrubber-rating-string #""))))))

(comment
  (solve-b)
  ; 4636702
  )
