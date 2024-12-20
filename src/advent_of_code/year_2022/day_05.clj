(ns advent-of-code.year-2022.day-05
  (:require [ysera.test :refer [is= is is-not]]))

(def test-input "    [D]    \n[N] [C]    \n[Z] [M] [P]\n 1   2   3 \n\nmove 1 from 2 to 1\nmove 3 from 1 to 3\nmove 2 from 2 to 1\nmove 1 from 1 to 2")
(def input (slurp "src/advent_of_code/year_2022/inputs/day05.txt"))

(defn parse-input
  [input size]
  (let [[first-half second-half] (clojure.string/split input #"\n\n")
        instructions (map (fn [line]
                            (->> line
                                 (re-seq #"\d+")
                                 (map read-string)))
                          (clojure.string/split-lines second-half))
        configuration (->> (clojure.string/split-lines first-half)
                           (drop-last)
                           (reduce (fn [a line]
                                     (reduce (fn [a i]
                                               (let [start-i (* i 4)
                                                     end-i (+ start-i 3)
                                                     my-part (subs line start-i end-i)
                                                     letter (re-find #"\w" my-part)]
                                                 (if letter
                                                   (update a i conj letter)
                                                   a)))
                                             a
                                             (range size)))
                                   (into [] (repeat size (list))))
                           (map reverse)
                           (into []))]
    [configuration instructions]))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input 3) "CMZ"))}
  [input size]
  (let [[configuration instructions] (parse-input input size)]
    (->> (reduce (fn [a [n from to]]
                   (let [from-i (dec from)
                         to-i (dec to)]
                     (reduce (fn [a _]
                               (let [letter-to-move (first (get a from-i))]
                                 (-> a
                                     (update from-i (partial drop 1))
                                     (update to-i conj letter-to-move))))
                             a
                             (range n))))
                 configuration
                 instructions)
         (map first)
         (reduce str))))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input 3) "MCD"))}
  [input size]
  (let [[configuration instructions] (parse-input input size)]
    (->> (reduce (fn [a [n from to]]
                   (let [from-i (dec from)
                         to-i (dec to)
                         letters-to-move (take n (get a from-i))]
                     (-> a
                         (update from-i (partial drop n))
                         (update to-i (partial concat letters-to-move)))))
                 configuration
                 instructions)
         (map first)
         (reduce str))))

(comment
  (time (part-1 input 9))
  ; CWMTGHBDW
  ;; "Elapsed time: 2.227875 msecs"

  (time (part-2 input 9))
  ; SSCGWJCRB
  ;; "Elapsed time: 3.460084 msecs"
  )

