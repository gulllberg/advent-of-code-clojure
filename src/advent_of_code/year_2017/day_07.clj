(ns advent-of-code.year-2017.day-07
  (:require [ysera.test :refer [is= is is-not]]
            [ysera.collections :refer [seq-contains?]]))

(def input (slurp "src/advent_of_code/year_2017/inputs/day07.txt"))

(defn part-1
  [input]
  (let [programs (clojure.string/split input #"\n")
        dependencies (reduce (fn [dependencies program]
                               (let [program-dependencies (clojure.string/split program #"->")
                                     program-dependencies-list (if (= (count program-dependencies) 1)
                                                                 []
                                                                 (-> (nth program-dependencies 1)
                                                                     (clojure.string/trim)
                                                                     (clojure.string/split #", ")))]
                                 (clojure.set/union dependencies (into #{} program-dependencies-list))))
                             #{}
                             programs)
        names (map (fn [program]
                     (-> (clojure.string/split program #" ")
                         (nth 0)))
                   programs)]
    (-> (remove (fn [name] (seq-contains? dependencies name)) names)
        (first))))

(defn get-dependencies
  [programs program]
  (let [program-dependencies (clojure.string/split program #"->")
        program-dependencies-vector (if (= (count program-dependencies) 1)
                                      []
                                      (-> (nth program-dependencies 1)
                                          (clojure.string/trim)
                                          (clojure.string/split #", ")))]
    (filter (fn [program]
              (seq-contains? program-dependencies-vector (-> (clojure.string/split program #" ")
                                                             (nth 0))))
            programs)))

(defn get-weight
  [program]
  (-> (clojure.string/split program #" ")
      (second)
      (read-string)
      (nth 0)))

(defn get-total-weight
  [programs program]
  (apply +
         (get-weight program)
         (map (fn [dependency]
                (get-total-weight programs dependency))
              (get-dependencies programs program))))

(defn part-2
  [input]
  (let [programs (clojure.string/split input #"\n")
        unbalanced-program (->> programs
                                (filter (fn [program]
                                          (let [dependencies (get-dependencies programs program)]
                                            (and (> (count dependencies) 0)
                                                 (apply not= (map (fn [dependency]
                                                                    (get-total-weight programs dependency))
                                                                  dependencies))))))
                                (sort-by (fn [program]
                                           (get-total-weight programs program)))
                                (first))
        unbalanced-program-dependencies (get-dependencies programs unbalanced-program)
        sorted-unbalanced-program-dependencies (sort-by (fn [dependency]
                                                          (get-total-weight programs dependency))
                                                        unbalanced-program-dependencies)
        sorted-unbalanced-program-dependencies-weights (map (fn [dependency]
                                                              (get-total-weight programs dependency))
                                                            sorted-unbalanced-program-dependencies)]
    (if (= (first sorted-unbalanced-program-dependencies-weights)
           (second sorted-unbalanced-program-dependencies-weights))
      (- (get-weight (last sorted-unbalanced-program-dependencies))
         (- (apply max sorted-unbalanced-program-dependencies-weights)
            (apply min sorted-unbalanced-program-dependencies-weights)))
      (+ (get-weight (first sorted-unbalanced-program-dependencies))
         (- (apply max sorted-unbalanced-program-dependencies-weights)
            (apply min sorted-unbalanced-program-dependencies-weights))))))

(comment
  (time (part-1 input))
  ;; hlqnsbe
  ;; "Elapsed time: 51.620083 msecs"

  (time (part-2 input))
  ;; 1993
  ;; "Elapsed time: 1903.368208 msecs"
  )