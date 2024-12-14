(ns advent-of-code.year_2018.day_08
  (:require [clojure.test]))

(def input (slurp "src/advent_of_code/year_2018/inputs/day08.txt"))

(defn recursive-sum-metadata-a
  {:test (fn []
           (clojure.test/is (= (first (recursive-sum-metadata-a (map read-string (clojure.string/split "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2" #" ")))) 138))
           (clojure.test/is (= (first (recursive-sum-metadata-a (map read-string (clojure.string/split "0 3 10 11 12" #" ")))) 33))
           (clojure.test/is (= (first (recursive-sum-metadata-a (map read-string (clojure.string/split "1 1 0 1 99 2" #" ")))) 101)))}
  [tree-list]
  (let [[[number-of-children number-of-metadata] tail] (split-at 2 tree-list)]
    (if (= number-of-children 0)
      (let [[metadata tail] (split-at number-of-metadata tail)]
        [(apply + metadata) tail])
      (let [[children-metadata-sum tail] (reduce (fn [[metadata-sum tree-list] _]
                                                   (let [[children-metadata-sum tail] (recursive-sum-metadata-a tree-list)]
                                                     [(+ metadata-sum children-metadata-sum) tail]))
                                                 [0 tail]
                                                 (range number-of-children))
            [metadata tail] (split-at number-of-metadata tail)]
        [(+ (apply + metadata) children-metadata-sum) tail]))))

(defn solve-a
  []
  (first (recursive-sum-metadata-a (map read-string (clojure.string/split input #" ")))))

(comment
  (solve-a)
  ;; 48496
  )

(defn recursive-sum-metadata-b
  {:test (fn []
           (clojure.test/is (= (first (recursive-sum-metadata-b (map read-string (clojure.string/split "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2" #" ")))) 66))
           (clojure.test/is (= (first (recursive-sum-metadata-b (map read-string (clojure.string/split "0 3 10 11 12" #" ")))) 33))
           (clojure.test/is (= (first (recursive-sum-metadata-b (map read-string (clojure.string/split "1 1 0 1 99 2" #" ")))) 0)))}
  [tree-list]
  (let [[[number-of-children number-of-metadata] tail] (split-at 2 tree-list)]
    (if (= number-of-children 0)
      (let [[metadata tail] (split-at number-of-metadata tail)]
        [(apply + metadata) tail])
      (let [[children-values tail] (reduce (fn [[child-values tree-list] _]
                                                   (let [[child-value tail] (recursive-sum-metadata-b tree-list)]
                                                     [(conj child-values child-value) tail]))
                                                 [[] tail]
                                                 (range number-of-children))
            [metadata tail] (split-at number-of-metadata tail)]
        [(apply + (map (fn [metadatum]
                         (get children-values (dec metadatum) 0))
                       metadata)) tail]))))

(defn solve-b
  []
  (first (recursive-sum-metadata-b (map read-string (clojure.string/split input #" ")))))

(comment
  (solve-b)
  ;; 32850
  )
