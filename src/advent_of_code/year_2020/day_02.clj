(ns advent-of-code.year-2020.day-02
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2020/inputs/day02.txt"))

; https://adventofcode.com/2020/day/2

(defn check-password-a
  {:test (fn []
           (is (check-password-a 1 3 "a" "abcde"))
           (is-not (check-password-a 1 3 "b" "cdefg"))
           (is (check-password-a 2 9 "c" "ccccccccc")))}
  [min max char password]
  (let [n-char-in-password (->> password
                                (filter (fn [c]
                                          (= (str c) char)))
                                (count))]
    (<= min n-char-in-password max)))

(defn solve-a
  []
    (let [lines (clojure.string/split-lines input)]
    (->> lines
         (filter (fn [l]
                   (let [parts (clojure.string/split l #" ")
                         numbers (clojure.string/split (nth parts 0) #"-")
                         min (read-string (nth numbers 0))
                         max (read-string (nth numbers 1))
                         char (str (nth (nth parts 1) 0))
                         password (nth parts 2)]
                     (check-password-a min max char password))))
         (count))))

(comment
  (solve-a)
  ; 477
  )

(defn check-password-b
  {:test (fn []
           (is (check-password-b 1 3 (nth "a" 0) "abcde"))
           (is-not (check-password-b 1 3 (nth "b" 0) "cdefg"))
           (is-not (check-password-b 2 9 (nth "c" 0) "ccccccccc")))}
  [p1 p2 char password]
  (let [p1-check (= (nth password (dec p1)) char)
        p2-check (= (nth password (dec p2)) char)]
    (not= p1-check p2-check)))

(defn solve-b
  []
  (let [lines (clojure.string/split-lines input)]
    (->> lines
         (filter (fn [l]
                   (let [parts (clojure.string/split l #" ")
                         numbers (clojure.string/split (nth parts 0) #"-")
                         p1 (read-string (nth numbers 0))
                         p2 (read-string (nth numbers 1))
                         char (nth (nth parts 1) 0)
                         password (nth parts 2)]
                     (check-password-b p1 p2 char password))))
         (count))))

(comment
  (solve-b)
  ; 686
  )
