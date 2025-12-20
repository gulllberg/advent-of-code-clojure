(ns advent-of-code.year-2025.day-11
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2025/inputs/day11.txt"))
(def test-input "aaa: you hhh\nyou: bbb ccc\nbbb: ddd eee\nccc: ddd eee fff\nddd: ggg\neee: out\nfff: out\nggg: out\nhhh: ccc fff iii\niii: out")
(def test-input-2 "svr: aaa bbb\naaa: fft\nfft: ccc\nbbb: tty\ntty: ccc\nccc: ddd eee\nddd: hub\nhub: fff\neee: dac\ndac: fff\nfff: ggg hhh\nggg: out\nhhh: out")

(defn parse-input
  {:test (fn []
           (is= (parse-input test-input)
                {"aaa" #{"you" "hhh"}
                 "bbb" #{"eee" "ddd"}
                 "ccc" #{"eee" "ddd" "fff"}
                 "ddd" #{"ggg"}
                 "eee" #{"out"}
                 "fff" #{"out"}
                 "ggg" #{"out"}
                 "hhh" #{"ccc" "fff" "iii"}
                 "iii" #{"out"}
                 "you" #{"ccc" "bbb"}}))}
  [input]
  (->> (clojure.string/split-lines input)
       (reduce (fn [a line]
                 (let [devices (re-seq #"\w+" line)]
                   (assoc a (first devices) (set (rest devices)))))
               {})))

(defn find-paths
  {:test (fn []
           (is= (find-paths (parse-input test-input) "you" "out")
                #{["you" "bbb" "ddd" "ggg" "out"]
                  ["you" "bbb" "eee" "out"]
                  ["you" "ccc" "ddd" "ggg" "out"]
                  ["you" "ccc" "eee" "out"]
                  ["you" "ccc" "fff" "out"]}))}
  [connections start end]
  (loop [ongoing-paths (list [start])
         finished-paths #{}]
    (if (empty? ongoing-paths)
      finished-paths
      (let [[ongoing-paths finished-paths] (reduce (fn [[ongoing-paths finished-paths] ongoing-path]
                        (let [continuations (get connections (last ongoing-path))
                              finish-continuation (some (fn [device]
                                                          (= device end))
                                                        continuations)
                              ongoing-continuations (remove (fn [device]
                                                              (= device end))
                                                            continuations)]
                          [(concat ongoing-paths (map (fn [device]
                                                        (conj ongoing-path device))
                                                      ongoing-continuations))
                           (if finish-continuation
                             (conj finished-paths (conj ongoing-path end))
                             finished-paths)]))
                      [(list) finished-paths]
                      ongoing-paths)]
        (recur ongoing-paths finished-paths)))))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 5))}
  [input]
  (-> (parse-input input)
      (find-paths "you" "out")
      (count)))

(defn get-number-of-paths
  [connections done goal]
  (loop [remaining-connections connections
         done done]
    (if (contains? done goal)
      done
      (let [[remaining-connections done] (reduce-kv (fn [[remaining-connections new-done] device outputs]
                                                      (let [ready-outputs (keep (fn [output]
                                                                                  (get done output nil))
                                                                                outputs)]
                                                        (if (= (count ready-outputs) (count outputs))
                                                          [(dissoc remaining-connections device) (assoc new-done device (reduce + ready-outputs))]
                                                          [remaining-connections new-done])))
                                                    [remaining-connections done]
                                                    remaining-connections)]
        (recur remaining-connections done)))))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input-2) 2))}
  [input]
  (let [connections (parse-input input)]
    (-> (reduce (fn [done goal]
                  (->> (get-number-of-paths connections done goal)
                       (reduce-kv (fn [a k v]
                                    (if (= k goal)
                                      (assoc a k v)
                                      (assoc a k 0)))
                                  {})))
                {"out" 1}
                ["dac" "fft" "svr"])
        (get "svr"))))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 18.007125 msecs"
  ;=> 791

  (time (part-2 input))
  ;; "Elapsed time: 11.1 msecs"
  ;=> 520476725037672
  )
