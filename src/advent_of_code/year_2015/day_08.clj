(ns advent-of-code.year-2015.day-08
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2015/inputs/day08.txt"))
(def test-input "\"\"\n\"abc\"\n\"aaa\\\"aaa\"\n\"\\x27\"")

(defn get-number-of-in-memory-characters
  [s]
  (let [num-double-backslash (count (re-seq #"\\\\" s))
        filtered-s (clojure.string/replace s #"\\\\" "")
        num-quote (count (re-seq #"\\\"" filtered-s))
        filtered-s (clojure.string/replace filtered-s #"\\\"" "")
        num-ascii (count (re-seq #"\\x[\da-f]{2}" filtered-s))
        filtered-s (clojure.string/replace filtered-s #"\\x[\da-f]{2}" "")]
    (+ (count filtered-s)
       ;; starting/ending quote
       -2
       num-double-backslash
       num-quote
       num-ascii)))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 12))}
  [input]
  (->> (clojure.string/split-lines input)
       (reduce (fn [a s]
                 (+ a (- (count s) (get-number-of-in-memory-characters s))))
               0)))

(defn get-number-of-encoded-characters
  [s]
  (let [num-backslash (count (re-seq #"\\" s))
        filtered-s (clojure.string/replace s #"\\" "")
        num-quote (count (re-seq #"\"" filtered-s))
        filtered-s (clojure.string/replace filtered-s #"\"" "")]
    (+ (count filtered-s)
       ;; starting/ending quote
       2
       (* 2 num-backslash)
       (* 2 num-quote))))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input) 19))}
  [input]
  (->> (clojure.string/split-lines input)
       (reduce (fn [a s]
                 (+ a (- (get-number-of-encoded-characters s) (count s))))
               0)))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 0.819542 msecs"
  ;=> 1333

  (time (part-2 input))
  ;; "Elapsed time: 0.787917 msecs"
  ;=> 2046
  )
