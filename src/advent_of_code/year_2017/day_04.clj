(ns advent-of-code.year-2017.day-04
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2017/inputs/day04.txt"))

(defn valid-passphrase-1?
  [passphrase]
  (let [passphrase-list (clojure.string/split passphrase #" ")]
    (= (count passphrase-list)
       (->> passphrase-list
            (set)
            (count)))))

(defn part-1
  {:test (fn []
           (is= (part-1 input)
                386))}
  [input]
  (->> (clojure.string/split input #"\n")
       (filter valid-passphrase-1?)
       (count)))

(defn valid-passphrase-2?
  {:test (fn []
           (clojure.test/is (not (valid-passphrase-2? "ab ba")))
           (clojure.test/is (valid-passphrase-2? "ab ac")))}
  [passphrase]
  (let [passphrase-list (clojure.string/split passphrase #" ")]
    (= (count passphrase-list)
       (->> passphrase-list
            (map sort)
            (set)
            (count)))))

(defn part-2
  {:test (fn []
           (is= (part-2 input)
                208))}
  [input]
  (->> (clojure.string/split input #"\n")
       (filter valid-passphrase-2?)
       (count)))

(clojure.test/deftest day-4
  (clojure.test/is (= (part-1 input) 386))
  (clojure.test/is (= (part-2 input) 208)))

(comment
  (valid-passphrase-1? "aa bb bb")
  (time (part-1 input))
  ;; 386
  ;; "Elapsed time: 0.774541 msecs"

  (time (part-2 input))
  ;; 208
  ;; "Elapsed time: 4.495542 msecs"
  )