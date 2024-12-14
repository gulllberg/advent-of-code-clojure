(ns advent-of-code.year_2018.day_04
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2018/inputs/day04.txt"))

(defn get-sorted-input
  []
  (->> input
       (clojure.string/split-lines)
       (sort)))

(defn get-timestamp
  {:test (fn []
           (is= (get-timestamp "[1518-11-10 00:43] falls asleep")
                43)
           (is= (get-timestamp "[1518-11-10 00:09] falls asleep")
                9))}
  [instruction]
  (let [number-string (-> instruction
                          (clojure.string/split #"]")
                          (first)
                          (clojure.string/split #":")
                          (second))]
    (if (= (subs number-string 0 1) "0")
      (read-string (subs number-string 1))
      (read-string number-string))))

(defn get-guard-id
  {:test (fn []
           (is= (get-guard-id "[1518-11-09 23:58] Guard #853 begins shift")
                "#853"))}
  [instruction]
  (as-> instruction $
        (clojure.string/split $ #" ")
        (some (fn [word]
                (when (clojure.string/includes? word "#")
                  word))
              $)))

(defn get-type-of-instruction
  {:test (fn []
           (is= (get-type-of-instruction "[1518-11-09 23:58] Guard #853 begins shift")
                :begin)
           (is= (get-type-of-instruction "[1518-11-23 00:07] falls asleep")
                :asleep)
           (is= (get-type-of-instruction "[1518-11-23 00:37] wakes up")
                :awake))}
  [instruction]
  (cond
    (clojure.string/includes? instruction "begins shift") :begin
    (clojure.string/includes? instruction "falls asleep") :asleep
    (clojure.string/includes? instruction "wakes up") :awake))

(defn get-guard-sleep-times
  []
  (first (reduce (fn [[guard-sleep-times active-guard asleep-timestamp] instruction]
                   (condp = (get-type-of-instruction instruction)
                     :begin [guard-sleep-times (get-guard-id instruction) nil]
                     :asleep [guard-sleep-times active-guard (get-timestamp instruction)]
                     :awake [(update guard-sleep-times active-guard (fn [sleep-times]
                                                                      (reduce (fn [sleep-times minute]
                                                                                (update sleep-times minute (fn [sleep-time]
                                                                                                             (if sleep-time
                                                                                                               (inc sleep-time)
                                                                                                               1))))
                                                                              sleep-times
                                                                              (range asleep-timestamp (get-timestamp instruction)))))
                             active-guard nil]))
                 [{} nil nil]
                 (get-sorted-input))))

(defn part-1
  []
  (let [guard-sleep-times (get-guard-sleep-times)
        most-sleepy-guard-id (->> guard-sleep-times
                                  (keys)
                                  (sort-by (fn [guard-id]
                                             (apply + (vals (get guard-sleep-times guard-id)))))
                                  (last))
        most-sleepy-minute (let [sleep-times (get guard-sleep-times most-sleepy-guard-id)]
                             (->> sleep-times
                                  (keys)
                                  (sort-by (fn [minute]
                                             (get sleep-times minute)))
                                  (last)))]
    (* (read-string (subs most-sleepy-guard-id 1)) most-sleepy-minute)))

(comment
  (time (part-1))
  ;; 101194
  ;; "Elapsed time: 6.286291 msecs"
  )

(defn part-2
  []
  (let [guard-sleep-times (get-guard-sleep-times)
        most-sleepy-guard-id (->> guard-sleep-times
                                  (keys)
                                  (sort-by (fn [guard-id]
                                             (apply max (vals (get guard-sleep-times guard-id)))))
                                  (last))
        most-sleepy-minute (let [sleep-times (get guard-sleep-times most-sleepy-guard-id)]
                             (->> sleep-times
                                  (keys)
                                  (sort-by (fn [minute]
                                             (get sleep-times minute)))
                                  (last)))]
    (* (read-string (subs most-sleepy-guard-id 1)) most-sleepy-minute)))

(comment
  (time (part-2))
  ;; 102095
  ;; "Elapsed time: 6.957208 msecs"
  )
