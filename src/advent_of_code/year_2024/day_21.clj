(ns advent-of-code.year-2024.day-21
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2024/inputs/day21.txt"))
(def test-input "029A\n980A\n179A\n456A\n379A")

(def directional-pad {[1 0] \^
                      [2 0] \A
                      [0 1] \<
                      [1 1] \v
                      [2 1] \>})

(def numerical-pad {[0 0] \7
                    [1 0] \8
                    [2 0] \9
                    [0 1] \4
                    [1 1] \5
                    [2 1] \6
                    [0 2] \1
                    [1 2] \2
                    [2 2] \3
                    [1 3] \0
                    [2 3] \A})

(defn get-position-of-character
  {:test (fn []
           (is= (get-position-of-character numerical-pad \2)
                [1 2]))}
  [pad character]
  (->> pad
       (some (fn [[p c]]
               (when (= c character)
                 p)))))

(def directions [[0 1] [0 -1] [-1 0] [1 0]])
(def direction->move {[0 1]  \v
                      [0 -1] \^
                      [-1 0] \<
                      [1 0]  \>})

(defn get-finished-sequences
  [boundary end-pos]
  (keep (fn [[pos s]]
          (when (= pos end-pos)
            s))
        boundary))

(defn get-shortest-sequences
  {:test (fn []
           (is= (->> (get-shortest-sequences numerical-pad [1 2] [2 1])
                     (into #{}))
                #{[\^ \>] [\> \^]}))}
  [pad start-pos end-pos]
  (loop [visited #{start-pos}
         boundary #{[start-pos []]}]
    (let [finished-sequences (get-finished-sequences boundary end-pos)]
      (if-not (empty? finished-sequences)
        finished-sequences
        (let [boundary (reduce (fn [boundary [pos current-seq]]
                                 (reduce (fn [boundary d]
                                           (let [new-pos (map + d pos)]
                                             (if (and (contains? pad new-pos)
                                                      (not (contains? visited new-pos)))
                                               (conj boundary [new-pos (conj current-seq (direction->move d))])
                                               boundary)))
                                         boundary
                                         directions))
                               #{}
                               boundary)
              visited (->> boundary
                           (map first)
                           (reduce conj visited))]
          (recur visited boundary))))))

(def move->directional-pad-position (clojure.set/map-invert directional-pad))

(def get-cost-of-move-sequence-directional-pad
  (memoize
    (fn [s remaining-robots]
      (if (zero? remaining-robots)
        (inc (count s))
        (->> (conj s \A)
             (cons \A)
             (map move->directional-pad-position)
             (partition 2 1)
             (map (fn [[start-pos end-pos]]
                    (->> (get-shortest-sequences directional-pad start-pos end-pos)
                         (map (fn [s]
                                (get-cost-of-move-sequence-directional-pad s (dec remaining-robots))))
                         (apply min))))
             (reduce +))))))

(clojure.test/deftest get-cost-of-move-sequence-directional-pad-test
  (is= (get-cost-of-move-sequence-directional-pad [\v \>] 0)
       3)
  (is= (get-cost-of-move-sequence-directional-pad [] 0)
       1)
  (is= (get-cost-of-move-sequence-directional-pad [] 1)
       1)
  (is= (get-cost-of-move-sequence-directional-pad [] 2)
       1)
  (is= (get-cost-of-move-sequence-directional-pad [\>] 1)
       4)
  (is= (get-cost-of-move-sequence-directional-pad [\<] 1)
       8)
  (is= (get-cost-of-move-sequence-directional-pad [\v] 1)
       6)
  (is= (get-cost-of-move-sequence-directional-pad [\^] 1)
       4)
  (is= (get-cost-of-move-sequence-directional-pad [\^] 2)
       12)
  (is= (get-cost-of-move-sequence-directional-pad [\v] 2)
       16)
  (is= (get-cost-of-move-sequence-directional-pad [\<] 2)
       18)
  (is= (get-cost-of-move-sequence-directional-pad [\>] 2)
       10)
  (is= (get-cost-of-move-sequence-directional-pad [\v \< \<] 1)
       10))

(defn get-cost-of-numerical-pad-move
  {:test (fn []
           (is= (get-cost-of-numerical-pad-move [2 3] [1 3] 2)
                18))}
  [start-pos end-pos number-of-directional-pad-robots]
  (->> (get-shortest-sequences numerical-pad start-pos end-pos)
       (map (fn [s]
              (get-cost-of-move-sequence-directional-pad s number-of-directional-pad-robots)))
       (apply min)))

(defn get-cost-of-code
  {:test (fn []
           (is= (get-cost-of-code "029A" 2)
                68))}
  [code number-of-directional-pad-robots]
  (->> code
       (seq)
       (cons \A)
       (map (fn [c]
              (get-position-of-character numerical-pad c)))
       (partition 2 1)
       (map (fn [[start-pos end-pos]]
              (get-cost-of-numerical-pad-move start-pos end-pos number-of-directional-pad-robots)))
       (reduce +)))

(defn get-code-numerical-value
  {:test (fn []
           (is= (get-code-numerical-value "029A")
                29))}
  [code]
  (->> code
       (re-find #"\d+")
       (str "10r")
       (read-string)))

(defn get-code-complexity
  {:test (fn []
           (is= (get-code-complexity "029A" 2) 1972))}
  [code number-of-directional-pad-robots]
  (* (get-code-numerical-value code)
     (get-cost-of-code code number-of-directional-pad-robots)))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 126384))}
  [input]
  (->> (clojure.string/split-lines input)
       (map (fn [code]
              (get-code-complexity code 2)))
       (reduce +)))

(defn part-2
  [input]
  (->> (clojure.string/split-lines input)
       (map (fn [code]
              (get-code-complexity code 25)))
       (reduce +)))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 7.4255 msecs"
  ;=> 184718

  (time (part-2 input))
  ;; "Elapsed time: 26.942083 msecs"
  ;=> 228800606998554
  )
