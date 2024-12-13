(ns advent-of-code.year-2021.day-08
  (:require [ysera.collections :refer [seq-contains?]]
            [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2021/inputs/day08.txt"))

(defn get-ten-digit-input
  [line]
  (drop-last 4 (re-seq #"\w+" line)))

(defn get-four-digit-output
  [line]
  (drop 10 (re-seq #"\w+" line)))

(defn part-1
  []
  (reduce (fn [sum line]
            (->> line
                 (get-four-digit-output)
                 (filter (fn [o]
                           (or (= (count o) 2)
                               (= (count o) 3)
                               (= (count o) 4)
                               (= (count o) 7))))
                 (count)
                 (+ sum)))
          0
          (clojure.string/split-lines input)))

(comment
  (time (part-1))
  ; 514
  ; "Elapsed time: 1.893042 msecs"
  )

;, https://stackoverflow.com/questions/10192602/return-first-item-in-a-map-list-sequence-that-satisfies-a-predicate
(defn find-first
  [pred coll]
  (first (filter pred coll)))

(defn get-key-by-value
  [map value]
  (find-first (fn [k]
                (= value (get map k)))
              (keys map)))

(defn get-other-key-with-same-value
  [map key]
  (first (filter (fn [k]
                   (and (not= k key)
                        (= (get map key) (get map k))))
                 (keys map))))

(defn get-number-by-letters
  {:test (fn []
           (is= (get-number-by-letters ["d" "e" "a" "f" "g" "b" "c"] "cdfeb") 5)
           (is= (get-number-by-letters ["d" "e" "a" "f" "g" "b" "c"] "fcadb") 3)
           (is= (get-number-by-letters ["d" "e" "a" "f" "g" "b" "c"] "cdbaf") 3))}
  [configuration letters]
  (condp = (map (fn [position]
                  (seq-contains? (clojure.string/split letters #"") position))
                configuration)
    [true true true false true true true] 0
    [false false true false false true false] 1
    [true false true true true false true] 2
    [true false true true false true true] 3
    [false true true true false true false] 4
    [true true false true false true true] 5
    [true true false true true true true] 6
    [true false true false false true false] 7
    [true true true true true true true] 8
    [true true true true false true true] 9
    nil))

(defn configuration-works?
  [configuration line]
  (= (reduce (fn [found-numbers letters]
               (conj found-numbers (get-number-by-letters configuration letters)))
             #{}
             line)
     #{0 1 2 3 4 5 6 7 8 9}))

(defn get-configuration
  {:test (fn []
           (is= (get-configuration (clojure.string/split "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab" #" "))
                ["d" "e" "a" "f" "g" "b" "c"]))}
  [line]
  (let [letter-counts (reduce (fn [letter-counts letter]
                                (update letter-counts letter inc))
                              {"a" 0
                               "b" 0
                               "c" 0
                               "d" 0
                               "e" 0
                               "f" 0
                               "g" 0}
                              (flatten (map (fn [letters-of-digit]
                                              (clojure.string/split letters-of-digit #""))
                                            line)))
        top-left (get-key-by-value letter-counts 6)
        bottom-left (get-key-by-value letter-counts 4)
        bottom-right (get-key-by-value letter-counts 9)
        ;; top and top-right share count
        top-first-guess (get-key-by-value letter-counts 8)
        top-second-guess (get-other-key-with-same-value letter-counts top-first-guess)
        ;; center and bottom share count
        center-first-guess (get-key-by-value letter-counts 7)
        center-second-guess (get-other-key-with-same-value letter-counts center-first-guess)]
    (find-first (fn [configuration]
                  (configuration-works? configuration line))
                (map (fn [i]
                       [(if (< i 2) top-first-guess top-second-guess)
                        top-left
                        (if-not (< i 2) top-first-guess top-second-guess)
                        (if (even? i) center-first-guess center-second-guess)
                        bottom-left
                        bottom-right
                        (if-not (even? i) center-first-guess center-second-guess)])
                     (range 4)))))

;; https://stackoverflow.com/questions/5057047/how-to-do-exponentiation-in-clojure
(defn exp [x n]
  (reduce * (repeat n x)))

(defn get-four-digit-number-from-output
  {:test (fn []
           (is= (get-four-digit-number-from-output ["d" "e" "a" "f" "g" "b" "c"] ["cdfeb" "fcadb" "cdfeb" "cdbaf"]) 5353))}
  [configuration four-digit-output]
  (reduce (fn [a i]
            (+ a
               (* (exp 10 i)
                  (get-number-by-letters configuration
                                         (nth (reverse four-digit-output) i)))))
          0
          (range 4)))

(defn part-2
  []
  (reduce (fn [sum line]
            (let [configuration (get-configuration (get-ten-digit-input line))]
              (+ sum (get-four-digit-number-from-output configuration (get-four-digit-output line)))))
          0
          (clojure.string/split-lines input)))

(comment
  (time (part-2))
  ; 1012272
  ; "Elapsed time: 75.714125 msecs"
  )
