(ns advent-of-code.year-2019.day-24
  (:require [ysera.test :refer [is= is is-not]]
            [advent-of-code.grid :refer [parse-grid get-neighbours]]
            [clojure.math]))

(def input (slurp "src/advent_of_code/year_2019/inputs/day24.txt"))
(def test-input "....#\n#..#.\n#..##\n..#..\n#....")

(defn parse-input
  [input]
  (->> (parse-grid input)
       (reduce-kv (fn [a p c]
                    (if (= c \#)
                      (conj a p)
                      a))
                  #{})))

(defn count-neighbours
  [bugs position]
  (->> (get-neighbours position)
       (filter bugs)
       (count)))

(defn get-next-minute
  [bugs]
  (->> (for [i (range 5)
             j (range 5)]
         [i j])
       (reduce (fn [a position]
                 (let [neighbours-count (count-neighbours bugs position)
                       is-bug (contains? bugs position)]
                   (if (or (and is-bug
                                (= neighbours-count 1))
                           (and (not is-bug)
                                (<= 1 neighbours-count 2)))
                     (conj a position)
                     a)))
               #{})))

(defn find-repetition
  [bugs]
  (loop [bugs bugs
         previous #{bugs}]
    (let [bugs (get-next-minute bugs)]
      (if (contains? previous bugs)
        bugs
        (recur bugs (conj previous bugs))))))

(defn biodiversity-points
  [[i j]]
  (long (clojure.math/pow 2 (+ (* i 5) j))))

(defn get-biodiversity
  [bugs]
  (->> bugs
       (map biodiversity-points)
       (reduce +)))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 2129920))}
  [input]
  (-> (parse-input input)
      (find-repetition)
      (get-biodiversity)))

;; This might be my most overengineered solution yet...
(def recursive-neighbours-configuration [{:condition  {:extract-fn first :value 0}
                                          :neighbours {:level-diff -1 :positions [[1 2]]}}
                                         {:condition  {:extract-fn first :value 4}
                                          :neighbours {:level-diff -1 :positions [[3 2]]}}
                                         {:condition  {:extract-fn second :value 0}
                                          :neighbours {:level-diff -1 :positions [[2 1]]}}
                                         {:condition  {:extract-fn second :value 4}
                                          :neighbours {:level-diff -1 :positions [[2 3]]}}
                                         ;; Inner
                                         {:condition {:extract-fn identity :value [1 2]}
                                          :neighbours {:level-diff 1 :positions (map (fn [j] [0 j]) (range 5))}}
                                         {:condition {:extract-fn identity :value [3 2]}
                                          :neighbours {:level-diff 1 :positions (map (fn [j] [4 j]) (range 5))}}
                                         {:condition {:extract-fn identity :value [2 1]}
                                          :neighbours {:level-diff 1 :positions (map (fn [i] [i 0]) (range 5))}}
                                         {:condition {:extract-fn identity :value [2 3]}
                                          :neighbours {:level-diff 1 :positions (map (fn [i] [i 4]) (range 5))}}])

(defn construct-recursive-neighbours-mapping
  []
  (->> (for [i (range 5)
             j (range 5)]
         [i j])
       (reduce (fn [a position]
                 (let [matching (filter (fn [{{extract-fn :extract-fn value :value} :condition}]
                                          (= (extract-fn position) value))
                                        recursive-neighbours-configuration)]
                   (if (empty? matching)
                     a
                     (assoc a position (fn [bugs level]
                                         (->> matching
                                              (map (fn [{{level-diff :level-diff positions :positions} :neighbours}]
                                                     (->> positions
                                                          (map (fn [p]
                                                                 (if (get-in bugs [(+ level level-diff) p])
                                                                   1
                                                                   0)))
                                                          (reduce +))))
                                              (reduce +)))))))
               {})))

(def recursive-neighbours-mapping (construct-recursive-neighbours-mapping))

(defn count-recursive-neighbours
  [bugs level position]
  (if-not (contains? recursive-neighbours-mapping position)
    0
    ((get recursive-neighbours-mapping position) bugs level)))

(defn count-neighbours-2
  [bugs level position]
  (let [regular-neighbours (->> (get-neighbours position)
                                (filter (get bugs level #{}))
                                (count))]
    (+ (count-recursive-neighbours bugs level position) regular-neighbours)))

(defn get-levels-to-check
  [bugs]
  (let [current-levels (keys bugs)]
    [(dec (apply min current-levels)) (inc (apply max current-levels))]))

(defn get-next-minute-2
  [bugs]
  (let [[min-level max-level] (get-levels-to-check bugs)]
    (->> (for [level (range min-level (inc max-level))
               i (range 5)
               j (range 5)
               :when (not= [i j] [2 2])]
           [level [i j]])
         (reduce (fn [a [level position]]
                   (let [neighbours-count (count-neighbours-2 bugs level position)
                         is-bug (get-in bugs [level position])]
                     (if (or (and is-bug
                                  (= neighbours-count 1))
                             (and (not is-bug)
                                  (<= 1 neighbours-count 2)))
                       (update a level (fnil conj #{}) position)
                       a)))
                 {}))))

(defn simulate-n-minutes
  [bugs n]
  (loop [steps 0
         bugs bugs]
    (if (= n steps)
      bugs
      (recur (inc steps) (get-next-minute-2 bugs)))))

(defn count-bugs
  [bugs]
  (->> (vals bugs)
       (map count)
       (reduce +)))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input 10) 99))}
  [input minutes]
  (-> {0 (parse-input input)}
      (simulate-n-minutes minutes)
      (count-bugs)))

(comment
  (time (part-1 input))
  ;; Elapsed time: 1.631875 msecs"
  ;=> 18400817

  (time (part-2 input 200))
  ;; "Elapsed time: 1077.051875 msecs"
  ;=> 1944
  )
