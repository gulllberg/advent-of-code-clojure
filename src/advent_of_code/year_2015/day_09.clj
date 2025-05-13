(ns advent-of-code.year-2015.day-09
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2015/inputs/day09.txt"))
(def test-input "London to Dublin = 464\nLondon to Belfast = 518\nDublin to Belfast = 141")

(defn parse-input
  [input]
  (->> (clojure.string/split-lines input)
       (reduce (fn [a line]
                 (let [[_ l1 l2 d] (re-find #"(\w+) to (\w+) = (\d+)" line)
                       d (read-string d)
                       old-l1 (get a l1 {})
                       old-l2 (get a l2 {})]
                   (assoc a l1 (assoc old-l1 l2 d)
                            l2 (assoc old-l2 l1 d))))
               {})))

(defn travelling-salesman
  [the-map]
  (let [all-locations (into #{} (keys the-map))]
    (loop [boundary (reduce (fn [a k]
                              (assoc a [k #{k}] 0))
                            {}
                            all-locations)
           shortest-distance ##Inf]
      (if (empty? boundary)
        shortest-distance
        (let [[boundary shortest-distance] (reduce-kv (fn [[boundary shortest-distance] [location visited] d]
                                                        (if (= visited all-locations)
                                                          [boundary (min shortest-distance d)]
                                                          (let [possible-moves (->> (get the-map location)
                                                                                    (keys)
                                                                                    (remove visited))]
                                                            [(reduce (fn [boundary m]
                                                                       (let [new-key [m (conj visited m)]
                                                                             new-d (+ d (get-in the-map [location m]))]
                                                                         (update boundary new-key (fn [old-d]
                                                                                                    (min new-d (or old-d ##Inf))))))
                                                                     boundary
                                                                     possible-moves)
                                                             shortest-distance])))
                                                      [{} shortest-distance]
                                                      boundary)]
          (recur boundary shortest-distance))))))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 605))}
  [input]
  (travelling-salesman (parse-input input)))

(defn get-longest-distance
  [the-map]
  (let [all-locations (into #{} (keys the-map))]
    (loop [boundary (reduce (fn [a k]
                              (assoc a [k #{k}] 0))
                            {}
                            all-locations)
           longest-distance 0]
      (if (empty? boundary)
        longest-distance
        (let [[boundary longest-distance] (reduce-kv (fn [[boundary longest-distance] [location visited] d]
                                                        (if (= visited all-locations)
                                                          [boundary (max longest-distance d)]
                                                          (let [possible-moves (->> (get the-map location)
                                                                                    (keys)
                                                                                    (remove visited))]
                                                            [(reduce (fn [boundary m]
                                                                       (let [new-key [m (conj visited m)]
                                                                             new-d (+ d (get-in the-map [location m]))]
                                                                         (update boundary new-key (fn [old-d]
                                                                                                    (max new-d (or old-d 0))))))
                                                                     boundary
                                                                     possible-moves)
                                                             longest-distance])))
                                                      [{} longest-distance]
                                                      boundary)]
          (recur boundary longest-distance))))))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input) 982))}
  [input]
  (get-longest-distance (parse-input input)))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 5.597792 msecs"
  ;=> 141

  (time (part-2 input))
  ;; "Elapsed time: 5.256666 msecs"
  ;=> 736
  )
