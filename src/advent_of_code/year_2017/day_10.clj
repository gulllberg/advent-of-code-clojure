(ns advent-of-code.year-2017.day-10
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2017/inputs/day10.txt"))

(defn problem-10-perform-length
  {:test (fn []
           (is= (problem-10-perform-length [[0 1 2 3 4] 0 0] 3)
                [[2 1 0 3 4] 3 1])
           (is= (problem-10-perform-length [[2 1 0 3 4] 3 1] 4)
                [[4 3 0 1 2] 3 2])
           (is= (problem-10-perform-length [[4 3 0 1 2] 3 2] 1)
                [[4 3 0 1 2] 1 3])
           (is= (problem-10-perform-length [[4 3 0 1 2] 1 3] 5)
                [[3 4 2 1 0] 4 4]))}
  [[circular-list current-position skip-size] length]
  [(reduce (fn [new-circular-list steps-taken]
             (let [put-in-position (mod (+ current-position steps-taken) (count circular-list))
                   get-from-position (mod (- (+ current-position length) steps-taken 1) (count circular-list))]
               (assoc new-circular-list put-in-position (nth circular-list get-from-position))))
           circular-list
           (range length))
   (mod (+ current-position length skip-size) (count circular-list))
   (inc skip-size)])

(defn problem-10-compute-list
  {:test (fn []
           (is= (problem-10-compute-list [0 1 2 3 4] "3,4,1,5")
                [3 4 2 1 0]))}
  [circular-list lengths]
  (-> (reduce problem-10-perform-length [circular-list 0 0] (->> (clojure.string/split lengths #",")
                                                                 (map read-string)))
      (first)))

(defn problem-10a
  [input]
  (let [computed-list (problem-10-compute-list (into [] (range 256)) input)]
    (* (first computed-list) (second computed-list))))

(defn problem-10b
  [input]
  (let [modified-input (-> (mapv int input)
                           (conj 17, 31, 73, 47, 23))
        sparse-hash (-> (reduce (fn [[circular-list current-position skip-size] _]
                                  (reduce problem-10-perform-length [circular-list current-position skip-size] modified-input))
                                [(into [] (range 256)) 0 0]
                                (range 64))
                        (first))
        dense-hash (->> (partition 16 sparse-hash)
                        (into [])
                        (mapv (fn [part] (apply bit-xor part))))]
    (->> dense-hash
         (mapv (fn [part] (format "%02x" part)))
         (apply str))))

(comment
  (problem-10a input)
  ;; 4114
  (problem-10b input)
  ;; 2f8c3d2100fdd57cec130d928b0fd2dd
  )