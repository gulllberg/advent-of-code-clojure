(ns advent-of-code.year-2015.day-19
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2015/inputs/day19.txt"))
(def test-input "H => HO\nH => OH\nO => HH")
(def test-input-2 "e => H\ne => O\nH => HO\nH => OH\nO => HH")

(defn parse-input
  [input]
  (let [[replacements molecule] (clojure.string/split input #"\n\n")]
    [(->> (clojure.string/split-lines replacements)
          (map (fn [line]
                 (let [[_ from to] (re-find #"(\w+) => (\w+)" line)]
                   [from to]))))
     molecule]))

(defn insert-at-index
  {:test (fn []
           (is= (insert-at-index (list 1 2 3 4 5) 3 6) (list 1 2 3 6 4 5)))}
  [coll i x]
  (concat (take i coll) (list x) (drop i coll)))

(defn put-molecule-together
  [split-molecule possible-insert-positions from to i]
  (->> (reduce (fn [a j]
                 (if (= i j)
                   (insert-at-index a j to)
                   (insert-at-index a j from)))
               split-molecule
               (reverse possible-insert-positions))
       (reduce str)))

(defn get-all-possible-molecules
  {:test (fn []
           (let [[replacements _] (parse-input test-input)]
             (is= (count (get-all-possible-molecules replacements "HOH")) 4)
             (is= (count (get-all-possible-molecules replacements "HOHOHO")) 7)))}
  [replacements starting-molecule]
  (reduce (fn [a [from to]]
            ;; -1 in split to include trailing empty string
            (let [split-molecule (clojure.string/split starting-molecule (re-pattern from) -1)
                  possible-insert-positions (range 1 (count split-molecule))]
              (reduce (fn [a i]
                        (conj a (put-molecule-together split-molecule possible-insert-positions from to i)))
                      a
                      possible-insert-positions)))
          #{}
          replacements))

(defn part-1
  [input]
  (let [[replacements starting-molecule] (parse-input input)]
    (count (get-all-possible-molecules replacements starting-molecule))))

(defn test-permutation
  [replacements molecule]
  (loop [molecule molecule
         steps 0]
    (cond (= molecule "e") steps
          (nil? molecule) nil
          :else (let [next-molecule (->> replacements
                                         (keep (fn [[from to]]
                                                 (let [next-molecule (clojure.string/replace-first molecule to from)]
                                                   (when-not (= molecule next-molecule)
                                                     next-molecule))))
                                         (first))]
                  (recur next-molecule (inc steps))))))

;; This will (obviously) not work in the general case
(defn get-fewest-steps-random
    {:test (fn []
             (let [[replacements _] (parse-input test-input-2)]
               (is= (get-fewest-steps-random replacements "HOH") 3)
               (is= (get-fewest-steps-random replacements "HOHOHO") 6)))}
  [replacements molecule]
  (loop [replacements replacements]
    (if-let [steps (test-permutation replacements molecule)]
      steps
      (recur (shuffle replacements)))))

(defn part-2
  [input]
  (let [[replacements target-molecule] (parse-input input)]
    (get-fewest-steps-random replacements target-molecule)))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 234.783292 msecs"
  ;=> 576

  (time (part-2 input))
  ;; "Elapsed time: 11.594584 msecs"
  ;=> 207
  )

;; This was used in my original "no code" solution. (Counting some specific molecules is very handy...)
(defn count-atoms-in-molecule
  {:test (fn []
           (is= (count-atoms-in-molecule "HOH") 3)
           (is= (count-atoms-in-molecule "RnAr") 2))}
  [molecule]
  (count (re-seq #"[A-Z][a-z]?" molecule)))

(defn count-specific-atom-in-molecule
  {:test (fn []
           (is= (count-specific-atom-in-molecule "HOH" "H") 2)
           (is= (count-specific-atom-in-molecule "RnAr" "Rn") 1)
           (is= (count-specific-atom-in-molecule "CaCa" "Ca") 2)
           (is= (count-specific-atom-in-molecule "CaCa" "C") 0))}
  [molecule atom]
  (count (re-seq (re-pattern (str atom "(?![a-z])")) molecule)))