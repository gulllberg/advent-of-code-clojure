(ns advent-of-code.year-2017.day-15
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2017/inputs/day15.txt"))

(def gen-a-start (->> (re-seq #"\d+" input)
                      (map read-string)
                      (first)))
(def gen-b-start (->> (re-seq #"\d+" input)
                      (map read-string)
                      (second)))

(defn compare-numbers
  [n1 n2]
  (let [conv-fn (fn [n]
                  (->> n
                       (Integer/toBinaryString)
                       (concat "0000000000000000")
                       (take-last 16)))]
    (= (conv-fn n1) (conv-fn n2))))

(defn problem-15a
  []
  (let [gen-a-factor 16807
        gen-b-factor 48271
        divisor 2147483647]
    (-> (reduce (fn [[score gen-a gen-b] _]
                  (let [new-gen-a (mod (* gen-a gen-a-factor) divisor)
                        new-gen-b (mod (* gen-b gen-b-factor) divisor)]
                    [(if (compare-numbers new-gen-a new-gen-b) (inc score) score)
                     new-gen-a
                     new-gen-b]))
                [0 gen-a-start gen-b-start]
                (range 40000000))
        (first))))

(defn problem-15b []
  (let [gen-a-factor 16807
        gen-a-multiple 4
        gen-b-factor 48271
        gen-b-multiple 8
        divisor 2147483647
        gen-a-numbers (loop [gen-a-numbers []
                             gen-a-current gen-a-start]
                        (if (= (count gen-a-numbers) 5000000)
                          gen-a-numbers
                          (let [new-gen-a (mod (* gen-a-current gen-a-factor) divisor)]
                            (recur (if (= (mod new-gen-a gen-a-multiple) 0) (conj gen-a-numbers new-gen-a) gen-a-numbers)
                                   new-gen-a))))
        gen-b-numbers (loop [gen-b-numbers []
                             gen-b-current gen-b-start]
                        (if (= (count gen-b-numbers) 5000000)
                          gen-b-numbers
                          (let [new-gen-b (mod (* gen-b-current gen-b-factor) divisor)]
                            (recur (if (= (mod new-gen-b gen-b-multiple) 0) (conj gen-b-numbers new-gen-b) gen-b-numbers)
                                   new-gen-b))))]
    (reduce (fn [score index]
              (if (compare-numbers (nth gen-a-numbers index) (nth gen-b-numbers index))
                (inc score)
                score))
            0
            (range 5000000)))
  )

(comment
  ;; 619 (slow)
  (problem-15a)

  ;; 290 (slow maybe)
  (problem-15b)
  )