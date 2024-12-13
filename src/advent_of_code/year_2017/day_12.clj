(ns advent-of-code.year-2017.day-12
  (:require [ysera.test :refer [is= is is-not]]
            [ysera.collections :refer [seq-contains?]]))

(def input (slurp "src/advent_of_code/year_2017/inputs/day12.txt"))

(defn get-system [connections partial-system node]
  (if (seq-contains? partial-system node)
    partial-system
    (reduce (fn [partial-system node]
              (get-system connections partial-system node))
            (conj partial-system node)
            (get connections node))))


(defn part-1
  {:test (fn []
           (is= (part-1 "0 <-> 2\n1 <-> 1\n2 <-> 0, 3, 4\n3 <-> 2, 4\n4 <-> 2, 3, 6\n5 <-> 6\n6 <-> 4, 5")
                6))}
  [input]
  (as-> input $
        (clojure.string/split $ #"\n")
        (reduce (fn [connections line]
                  (assoc connections
                    (-> (clojure.string/split line #"<->")
                        (first)
                        (clojure.string/trim))
                    (as-> line $
                          (clojure.string/split $ #"<->")
                          (second $)
                          (clojure.string/split $ #",")
                          (map clojure.string/trim $))))
                {}
                $)
        (get-system $ [] "0")
        (count $)))

(defn part-2
  {:test (fn []
           (is= (part-2 "0 <-> 2\n1 <-> 1\n2 <-> 0, 3, 4\n3 <-> 2, 4\n4 <-> 2, 3, 6\n5 <-> 6\n6 <-> 4, 5")
                2))}
  [input]
  (as-> input $
        (clojure.string/split $ #"\n")
        (reduce (fn [connections line]
                  (assoc connections
                    (-> (clojure.string/split line #"<->")
                        (first)
                        (clojure.string/trim))
                    (as-> line $
                          (clojure.string/split $ #"<->")
                          (second $)
                          (clojure.string/split $ #",")
                          (map clojure.string/trim $))))
                {}
                $)
        (map (fn [node]
               (-> (get-system $ [] node)
                   (sort)))
             (keys $))
        (distinct $)
        (count $)))

(comment
  ;; 175
  ;; "Elapsed time: 11.235833 msecs"
  (time (part-1 input))

  (time (part-2 input))
  ;; 213
  ;; "Elapsed time: 326.233042 msecs"
  )