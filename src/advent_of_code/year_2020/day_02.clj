(ns advent-of-code.year-2020.day-02
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2020/inputs/day02.txt"))

(defn check-password-1
  {:test (fn []
           (is (check-password-1 1 3 "a" "abcde"))
           (is-not (check-password-1 1 3 "b" "cdefg"))
           (is (check-password-1 2 9 "c" "ccccccccc")))}
  [min max char password]
  (let [n-char-in-password (->> password
                                (filter (fn [c]
                                          (= (str c) char)))
                                (count))]
    (<= min n-char-in-password max)))

(defn part-1
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
                     (check-password-1 min max char password))))
         (count))))

(comment
  (time (part-1))
  ; 477
  ; "Elapsed time: 4.754459 msecs"
  )

(defn check-password-2
  {:test (fn []
           (is (check-password-2 1 3 (nth "a" 0) "abcde"))
           (is-not (check-password-2 1 3 (nth "b" 0) "cdefg"))
           (is-not (check-password-2 2 9 (nth "c" 0) "ccccccccc")))}
  [p1 p2 char password]
  (let [p1-check (= (nth password (dec p1)) char)
        p2-check (= (nth password (dec p2)) char)]
    (not= p1-check p2-check)))

(defn part-2
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
                     (check-password-2 p1 p2 char password))))
         (count))))

(comment
  (time (part-2))
  ; 686
  ; "Elapsed time: 2.73075 msecs"
  )
