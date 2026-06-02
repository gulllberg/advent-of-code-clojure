(ns advent-of-code.year-2019.day-23
  (:require [ysera.test :refer [is= is is-not]]
            [advent-of-code.year-2019.intcode :refer [parse-program run-intcode-program continue-intcode-program]]))

;; With Tomas

(def input (slurp "src/advent_of_code/year_2019/inputs/day23.txt"))

(defn get-network-packets
  [computers]
  (->> computers
       (map :program-output)
       (remove empty?)
       (flatten)
       (partition 3)))

(defn send-packets
  [computers network-queue network-packets]
  (let [network-queue (reduce (fn [a [address X Y]]
                                (update a address (fnil conj []) [X Y]))
                              network-queue
                              network-packets)]
    [(->> computers
          (reduce-kv (fn [a address computer]
                       (assoc a address (continue-intcode-program (assoc computer :program-output []) (first (get network-queue address [[-1]])))))
                     []))
     (reduce (fn [a address]
               (update a address (fn [queue]
                                   (into [] (rest queue)))))
             network-queue
             (keys network-queue))]))

(defn part-1
  [input]
  (let [program (parse-program input)]
    (loop [computers (->> (range 50)
                          (mapv (fn [address]
                                  (run-intcode-program program [address -1]))))
           network-queue {}]
      (let [network-packets (get-network-packets computers)]
        (if-let [Y (some (fn [[address _ Y]]
                           (when (= address 255)
                             Y))
                         network-packets)]
          Y
          (let [[computers network-queue] (send-packets computers network-queue network-packets)]
            (recur computers network-queue)))))))

(defn idle?
  [network-queue network-packets]
  (->> (conj (vals network-queue) network-packets)
       (remove empty?)
       (empty?)))

(defn part-2
  [input]
  (let [program (parse-program input)]
    (loop [[computers network-queue] [(->> (range 50)
                                           (mapv (fn [address]
                                                   (run-intcode-program program [address -1]))))
                                      {}]
           nat nil
           last-delivered-nat nil]
      (let [network-packets (get-network-packets computers)
            new-nat (some (fn [[address X Y]]
                            (when (= address 255)
                              [X Y]))
                          network-packets)]
        (cond
          (idle? network-queue network-packets)
          (if (and last-delivered-nat (= (second last-delivered-nat) (second nat)))
            (second nat)
            (recur (send-packets computers network-queue [[0 (first nat) (second nat)]]) nat nat))

          new-nat (recur (send-packets computers network-queue (remove (fn [[address _ _]]
                                                                         (= address 255))
                                                                       network-packets))
                         new-nat
                         last-delivered-nat)
          :else
          (recur (send-packets computers network-queue network-packets) nat last-delivered-nat))))))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 26.935416 msecs"
  ;=> 22074

  (time (part-2 input))
  ;; "Elapsed time: 118.546083 msecs"
  ;=> 14257
  )
