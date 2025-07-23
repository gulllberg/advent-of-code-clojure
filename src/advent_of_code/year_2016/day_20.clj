(ns advent-of-code.year-2016.day-20
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2016/inputs/day20.txt"))
(def test-input "5-8\n0-2\n4-7")

(def max-ip 4294967295)
(def test-max-ip 9)

(defn parse-input
  [input]
  (->> (clojure.string/split-lines input)
       (reduce (fn [a line]
                 (let [number-strings (re-seq #"\d+" line)
                       [from to] (map read-string number-strings)]
                   (conj a [from to])))
               #{})))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 3))}
  [input]
  (loop [candidate-ip 0
         blocklist (parse-input input)]
    (let [relevant-blocklist (filter (fn [[from _]] (<= from candidate-ip)) blocklist)]
      (if (empty? relevant-blocklist)
        candidate-ip
        (let [max-blocked-ip (reduce (fn [max-blocked-ip [_ to]]
                                       (max max-blocked-ip to))
                                     candidate-ip
                                     relevant-blocklist)]
          (recur (inc max-blocked-ip)
                 (reduce (fn [a v]
                           (disj a v))
                         blocklist
                         ;; also need to remove any blocklist instructions with smaller to ip
                         ;; (i.e., a range completely contained within a range in relevant-blocklist)
                         (concat (filter (fn [[_ to]] (< to max-blocked-ip)) blocklist)
                                 relevant-blocklist))))))))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input test-max-ip) 2))}
  [input max-ip]
  (loop [candidate-ip 0
         blocklist (parse-input input)
         number-of-allowed-ips 0]
    (let [relevant-blocklist (filter (fn [[from _]] (<= from candidate-ip)) blocklist)]
      (cond
        ;; outside allowed range
        (> candidate-ip max-ip)
        number-of-allowed-ips

        ;; all blocklist instructions processed - remaining ips are ok
        (empty? blocklist)
        (+ number-of-allowed-ips (- max-ip candidate-ip) 1)

        ;; no (remaining unprocessed) blocklist hits the current candidate-ip - ips until next blocklist start are ok
        (empty? relevant-blocklist)
        (let [next-blocklist-start (reduce (fn [a [from _]]
                                             (min a from))
                                           max-ip
                                           blocklist)]
          (recur next-blocklist-start
                 blocklist
                 (+ number-of-allowed-ips (- next-blocklist-start candidate-ip))))

        ;; there is (at least) one blocklist instruction that hits the current candidate-ip - check how long it blocks for
        :else
        (let [max-blocked-ip (reduce (fn [max-blocked-ip [_ to]]
                                       (max max-blocked-ip to))
                                     0
                                     relevant-blocklist)]
          (recur (inc max-blocked-ip)
                 (reduce (fn [a v]
                           (disj a v))
                         blocklist
                         ;; also need to remove any blocklist instructions with smaller to ip
                         ;; (i.e., a range completely contained within a range in relevant-blocklist)
                         (concat (filter (fn [[_ to]] (< to max-blocked-ip)) blocklist)
                                 relevant-blocklist))
                 number-of-allowed-ips))))))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 12.497792 msecs"
  ;=> 31053880

  (time (part-2 input max-ip))
  ;; "Elapsed time: 39.558667 msecs"
  ;=> 117
  )
