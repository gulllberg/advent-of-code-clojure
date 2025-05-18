(ns advent-of-code.year-2015.day-24
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2015/inputs/day24.txt"))
(def test-input "1\n2\n3\n4\n5\n7\n8\n9\n10\n11")

;; Trying to always use the heaviest weight possible to generate the best solution has a lot of appeal.
;; 1. Using larger weights means fewer weights.
;; 2. n^2 > (n-1)(n+1), so larger weights means smaller quantum entanglement.
;; 3. Using fewer and larger weights (e.g. 3 instead of 2+1), should make it easier to complete subsequent groups.
;; This suggests just doing a depth first search. However, in the general case this does not necessarily work.
;; We need to also consider that pure dfs might not get the best solution,
;; and that given a (best) solution we still need to fill the other compartments successfully.
;; My original solution got all possibilities assuming just the largest weight, and tried them until the other groups could be completed.
;; This was a bit slow (2s for part 1, 0.5s for part 2 where the weight per compartment is smaller so fewer possibilities).
;; I then toyed around with various dfs variants.
;; Pure dfs works for the examples and part 1, but not part 2.
;; dfs and getting the first n solutions runs fast and finds the best solution for part 2 if you pick the right n.
;; Running dfs and stopping if you can no longer beat the current best will find the best solution,
;; but since you have no sequence if it turns out the other compartments cannot be finished you are stuck.
;; It turns out that in this problem we don't have that issue.
;; This solution (current one used) does run (a lot) slower than the dfs with n first. But noticeably faster than the initial attempt.

(defn compare-groups
  {:test (fn []
           (is (compare-groups [9 11] [10 9 1]))
           (is (compare-groups [9 11] nil))
           (is-not (compare-groups nil [10 9 1]))
           (is-not (compare-groups [10 8 2] [10 9 1])))}
  [g1 g2]
  (cond (nil? g2) true
        (nil? g1) false
        (< (count g1) (count g2)) true
        (> (count g1) (count g2)) false
        (< (reduce * g1) (reduce * g2)) true
        (> (reduce * g1) (reduce * g2)) false
        :else false))

(defn sort-groups
  [groups]
  (sort (fn [g1 g2]
          (cond (< (count g1) (count g2)) -1
                (> (count g1) (count g2)) 1
                (< (reduce * g1) (reduce * g2)) -1
                (> (reduce * g1) (reduce * g2)) 1
                :else 0))
        groups))

(defn dfs
  {:test (fn []
           (is= (dfs (->> (clojure.string/split-lines test-input)
                          (map read-string)
                          (sort)
                          (reverse)) 20)
                [9 11]))}
  [weights target-weight]
  (cond (zero? target-weight) []
        (or (neg? target-weight)
            (empty? weights)) nil
        :else (let [[weight & weights] weights]
                (if-let [used-weights (dfs weights (- target-weight weight))]
                  (conj used-weights weight)
                  (dfs weights target-weight)))))

(defn dfs-n-best
  {:test (fn []
           (is= (dfs-n-best (->> (clojure.string/split-lines test-input)
                          (map read-string)
                          (sort)
                          (reverse)) 20 1)
                [[9 11]]))}
  [weights target-weight n]
  (cond (zero? target-weight) [[]]
        (or (neg? target-weight)
            (empty? weights)) []
        :else (let [[weight & weights] weights
                    solutions-using-weight (map (fn [s]
                                                  (conj s weight))
                                                (dfs-n-best weights (- target-weight weight) n))]
                (if (>= (count solutions-using-weight) n)
                  (take n (sort-groups solutions-using-weight))
                  (reduce conj
                          solutions-using-weight
                          (dfs-n-best weights
                                      target-weight
                                      (- n (count solutions-using-weight))))))))
(defn dfs-exhaustive
  {:test (fn []
           (is= (dfs-exhaustive (->> (clojure.string/split-lines test-input)
                                     (map read-string)
                                     (sort)
                                     (reverse)) 20)
                [11 9]))}
  ([weights target-weight current-solution best-solution]
   (cond (compare-groups best-solution current-solution) best-solution
         (zero? target-weight) current-solution
         (or (neg? target-weight)
             (empty? weights)) best-solution
         :else (let [[weight & weights] weights]
                 (if-let [solution-using-weight (dfs-exhaustive weights
                                                                (- target-weight weight)
                                                                (conj current-solution weight)
                                                                best-solution)]
                   (dfs-exhaustive weights target-weight current-solution solution-using-weight)
                   (dfs-exhaustive weights target-weight current-solution best-solution)))))
  ([weights target-weight]
   (dfs-exhaustive weights target-weight [] nil)))

(defn get-n-partitions
  ([n weights partitions]
   (if (= 1 n)
     (conj partitions weights)
     (let [weights (reverse (sort weights))
           target-weight (/ (reduce + weights) n)
           partition (dfs-exhaustive weights target-weight)]
       (get-n-partitions (dec n) (remove (into #{} partition) weights) (conj partitions partition)))))
  ([n weights] (get-n-partitions n weights [])))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 99))}
  [input]
  (->> (clojure.string/split-lines input)
       (map read-string)
       (get-n-partitions 3)
       (sort-groups)
       (first)
       (reduce *)))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input) 44))}
  [input]
  (->> (clojure.string/split-lines input)
       (map read-string)
       (get-n-partitions 4)
       (sort-groups)
       (first)
       (reduce *)))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 516.091875 msecs"
  ;=> 10723906903

  (time (part-2 input))
  ;; "Elapsed time: 88.91375 msecs"
  ;=> 74850409
  )
