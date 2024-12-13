(ns advent-of-code.year-2017.day-04
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2017/inputs/day04.txt"))

(defn valid-passphrase-a?
  [passphrase]
  (let [passphrase-list (clojure.string/split passphrase #" ")]
    (= (count passphrase-list)
       (->> passphrase-list
            (set)
            (count)))))

(defn problem-4a
  {:test (fn []
           (is= (problem-4a input)
                386))}
  [input]
  (->> (clojure.string/split input #"\n")
       (filter valid-passphrase-a?)
       (count)))

(defn valid-passphrase-b?
  {:test (fn []
           (clojure.test/is (not (valid-passphrase-b? "ab ba")))
           (clojure.test/is (valid-passphrase-b? "ab ac")))}
  [passphrase]
  (let [passphrase-list (clojure.string/split passphrase #" ")]
    (= (count passphrase-list)
       (->> passphrase-list
            (map sort)
            (set)
            (count)))))

(defn problem-4b
  {:test (fn []
           (is= (problem-4b input)
                208))}
  [input]
  (->> (clojure.string/split input #"\n")
       (filter valid-passphrase-b?)
       (count)))

(clojure.test/deftest day-4
  (clojure.test/is (= (problem-4a input) 386))
  (clojure.test/is (= (problem-4b input) 208)))

(comment
  (valid-passphrase-a? "aa bb bb")
  (problem-4a input)
  ;; 386
  (problem-4b input)
  ;; 208
  )