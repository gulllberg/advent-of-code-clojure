(ns advent-of-code.year-2024.day-25
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2024/inputs/day25.txt"))
(def test-input "#####\n.####\n.####\n.####\n.#.#.\n.#...\n.....\n\n#####\n##.##\n.#.##\n...##\n...#.\n...#.\n.....\n\n.....\n#....\n#....\n#...#\n#.#.#\n#.###\n#####\n\n.....\n.....\n#.#..\n###..\n###.#\n###.#\n#####\n\n.....\n.....\n.....\n#....\n#.#..\n#.#.#\n#####")

(defn parse-key-or-lock
  [key-or-lock]
  (->> key-or-lock
       (rest)
       (reduce (fn [a line]
                 (reduce-kv (fn [a idx v]
                              (if (= v \#)
                                (update a idx inc)
                                a))
                            a
                            (into [] line)))
               [0 0 0 0 0])))

(defn parse-lock
  {:test (fn []
           (is= (parse-lock "#####\n.####\n.####\n.####\n.#.#.\n.#...\n.....")
                [0, 5, 3, 4, 3]))}
  [lock]
  (->> (clojure.string/split-lines lock)
       (parse-key-or-lock)))

(defn parse-key
  {:test (fn []
           (is= (parse-key ".....\n#....\n#....\n#...#\n#.#.#\n#.###\n#####")
                [5, 0, 2, 1, 3]))}
  [key]
  (->> (clojure.string/split-lines key)
       (reverse)
       (parse-key-or-lock)))

(defn lock?
  [key-or-lock]
  (= \# (first key-or-lock)))

(defn parse-input
  [input]
  (->> (clojure.string/split input #"\n\n")
       (reduce (fn [a key-or-lock]
                 (if (lock? key-or-lock)
                   (update a :locks conj (parse-lock key-or-lock))
                   (update a :keys conj (parse-key key-or-lock))))
               {:keys  #{}
                :locks #{}})))

(defn test-key-lock-combination
  [key lock]
  (->> (map + key lock)
       (some (fn [sum] (>= sum 6)))
       (not)))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 3))}
  [input]
  (let [keys-and-locks (parse-input input)]
    (reduce (fn [a lock]
              (+ a (->> (:keys keys-and-locks)
                        (filter (fn [k] (test-key-lock-combination k lock)))
                        (count))))
            0
            (:locks keys-and-locks))))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 35.219916 msecs"
  ;=> 3356
  )
