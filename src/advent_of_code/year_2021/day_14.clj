(ns advent-of-code.year-2021.day-14)

(def input (slurp "src/advent_of_code/year_2021/inputs/day14.txt"))
(def input-template (first (clojure.string/split input #"\n\n")))
(def input-rules (second (clojure.string/split input #"\n\n")))
(def test-input "NNCB\n\nCH -> B\nHH -> N\nCB -> H\nNH -> C\nHB -> C\nHC -> B\nHN -> C\nNN -> C\nBH -> H\nNC -> B\nNB -> B\nBN -> B\nBB -> N\nBC -> B\nCC -> N\nCN -> C")
(def test-input-template (first (clojure.string/split test-input #"\n\n")))
(def test-input-rules (second (clojure.string/split test-input #"\n\n")))

(defn create-rules-state
  [rules-input]
  (reduce (fn [state line]
            (let [[pair add] (re-seq #"\w+" line)]
              (assoc state pair add)))
          {}
          (clojure.string/split-lines rules-input)))

(defn one-step
  [polymer rules-state]
  (reduce (fn [new-polymer i]
            (let [pair-to-test (subs polymer i (+ i 2))]
              (str new-polymer (get rules-state pair-to-test) (second pair-to-test))))
          (str (first polymer))
          (range (dec (count polymer)))))

(defn n-steps
  [polymer rules-state n]
  (reduce (fn [polymer _]
            (one-step polymer rules-state))
          polymer
          (range n)))

(defn get-character-counts
  [polymer]
  (reduce (fn [counts c]
            (if (contains? counts c)
              (update counts c inc)
              (assoc counts c 1)))
          {}
          polymer))

(defn get-difference-biggest-smallest
  [character-counts]
  (let [[min-v max-v] (reduce (fn [[min-v max-v] v]
                                [(min min-v v) (max max-v v)])
                              [##Inf 0]
                              (vals character-counts))]
    (- max-v min-v)))

(defn part-1
  []
  (let [rules-state (create-rules-state input-rules)
        polymer (n-steps input-template rules-state 10)
        character-counts (get-character-counts polymer)]
    (get-difference-biggest-smallest character-counts)))

(comment
  (time (part-1))
  ; 2321
  ; "Elapsed time: 23.684916 msecs"
  )

;; Part 1 version too slow for part 2

(defn create-pairs-state
  [template]
  (reduce (fn [state i]
            (let [pair (subs template i (+ i 2))]
              (if (contains? state pair)
                (update state pair inc)
                (assoc state pair 1))))
          {}
          (range (dec (count template)))))

(defn add-pair
  [pairs-state pair cnt]
  (if (contains? pairs-state pair)
    (update pairs-state pair + cnt)
    (assoc pairs-state pair cnt)))

(defn one-step-2
  [pairs-state rules-state]
  (reduce (fn [new-pairs-state pair]
            (let [add (get rules-state pair)
                  first-new-pair (str (first pair) add)
                  second-new-pair (str add (second pair))
                  cnt (get pairs-state pair)]
              (-> new-pairs-state
                  (add-pair first-new-pair cnt)
                  (add-pair second-new-pair cnt))))
          {}
          (keys pairs-state)))

(defn add-key
  [counts c cnt]
  (if (contains? counts c)
    (update counts c + cnt)
    (assoc counts c cnt)))

(defn pairs-state->character-counts
  [pairs-state first-char last-char]
  (let [doubled-counts (reduce (fn [doubled-counts k]
                                 (let [first-c (first k)
                                       second-c (second k)
                                       cnt (get pairs-state k)]
                                   (-> doubled-counts
                                       (add-key first-c cnt)
                                       (add-key second-c cnt))))
                               {first-char 1
                                last-char  1}
                               (keys pairs-state))]
    (reduce (fn [counts k]
              (assoc counts k (/ (get doubled-counts k) 2)))
            {}
            (keys doubled-counts))))

(defn part-2
  []
  (let [rules-state (create-rules-state input-rules)
        pairs-state (reduce (fn [pairs-state _]
                              (one-step-2 pairs-state rules-state))
                            (create-pairs-state input-template)
                            (range 40))
        character-counts (pairs-state->character-counts pairs-state (first input-template) (last input-template))]
    (get-difference-biggest-smallest character-counts)))

(comment
  (time (part-2))
  ; 2399822193707
  ; "Elapsed time: 3.243792 msecs"
  )
