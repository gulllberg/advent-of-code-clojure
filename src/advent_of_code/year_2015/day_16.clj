(ns advent-of-code.year-2015.day-16
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2015/inputs/day16.txt"))
(def mfcsam-output "children: 3\ncats: 7\nsamoyeds: 2\npomeranians: 3\nakitas: 0\nvizslas: 0\ngoldfish: 5\ntrees: 3\ncars: 2\nperfumes: 1")

(defn parse-attributes
  [attributes]
  (reduce (fn [a attribute]
            (let [[k v] (clojure.string/split attribute #": ")]
              (assoc a k (read-string v))))
          {}
          attributes))

(def requirements (parse-attributes (clojure.string/split-lines mfcsam-output)))

(defn parse-input
  [input]
  (reduce (fn [a line]
            (let [[_ id attributes] (re-find #"Sue (\d+): (.*)" line)]
              (assoc a id (parse-attributes (clojure.string/split attributes #", ")))))
          {}
          (clojure.string/split-lines input)))

(defn possible-sue?
  [requirements sue-data]
  (every? (fn [k]
            (= (get sue-data k) (get requirements k)))
          (keys sue-data)))

(defn part-1
  [input]
  (let [sues (parse-input input)]
    (first (filter (fn [id]
                     (possible-sue? requirements (get sues id)))
                   (keys sues)))))

(defn possible-sue-2?
  [requirements sue-data]
  (every? (fn [k]
            (let [op (cond (contains? #{"cats" "trees"} k) >
                           (contains? #{"pomeranians" "goldfish"} k) <
                           :else =)]
              (op (get sue-data k) (get requirements k))))
          (keys sue-data)))

(defn part-2
  [input]
  (let [sues (parse-input input)]
    (first (filter (fn [id]
                     (possible-sue-2? requirements (get sues id)))
                   (keys sues)))))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 2.462958 msecs"
  ;=> "213"

  (time (part-2 input))
  ;; "Elapsed time: 6.041875 msecs"
  ;=> "323"
  )
