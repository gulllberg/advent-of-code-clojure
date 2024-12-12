(ns advent-of-code.year-2024.day-12
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2024/inputs/day12.txt"))
(def test-input "AAAA\nBBCD\nBBCC\nEEEC")
(def test-input-large "RRRRIICCFF\nRRRRIICCCF\nVVRRRCCFFF\nVVRCCCJFFF\nVVVVCJJCFE\nVVIVCCJJEE\nVVIIICJJEE\nMIIIIIJJEE\nMIIISIJEEE\nMMMISSJEEE")

(defn create-garden-map
  [input]
  (let [lines (into [] (clojure.string/split-lines input))]
    (reduce-kv (fn [a i line]
                 (reduce-kv (fn [a j c]
                              (assoc a [i j] c))
                            a
                            (into [] line)))
               {}
               lines)))

(def directions [[-1 0] [1 0] [0 -1] [0 1]])

(def garden-map (create-garden-map test-input))

(defn find-region
  {:test (fn []
           (is= (find-region (create-garden-map test-input)
                             [0 0])
                #{[0 0]
                  [0 1]
                  [0 2]
                  [0 3]})
           (is= (find-region (create-garden-map test-input)
                             [2 2])
                #{[1 2]
                  [2 2]
                  [2 3]
                  [3 3]}))}
  [garden-map position]
  (let [plant (get garden-map position)]
    (loop [region #{position}]
      (let [next-region
            (->> region
                 (map (fn [p]
                        (->> directions
                             (map (fn [d]
                                    (map + d p)))
                             (filter (fn [np] (= plant (get garden-map np))))
                             (remove region))))
                 (apply concat)
                 (remove empty?)
                 (into #{})
                 (clojure.set/union region))]
        (if (= next-region region)
          region
          (recur next-region))))))

(defn find-regions
  {:test (fn []
           (is= (find-regions (create-garden-map test-input))
                #{#{[1 3]}
                  #{[3 0] '(3 1) '(3 2)}
                  #{[1 0] '(1 1) '(2 0) '(2 1)}
                  #{[2 2] '(2 3) '(3 3) '(1 2)}
                  #{[0 0] '(0 3) '(0 2) '(0 1)}}))}
  [garden-map]
  (loop [garden-map garden-map
         regions #{}]
    (if (empty? garden-map)
      regions
      (let [[position _] (first garden-map)]
        (let [region (find-region garden-map position)]
          (recur (apply dissoc garden-map region)
                 (conj regions region)))))))

(defn get-perimeter
  [region]
  (->> region
       (map (fn [p]
              (->> directions
                   (map (fn [d]
                          (map + d p)))
                   (remove region))))
       (apply concat)))

(defn get-perimeter-length
  {:test (fn []
           (is= (get-perimeter-length #{[1 2]
                                        [2 2]
                                        [2 3]
                                        [3 3]})
                10))}
  [region]
  (count (get-perimeter region)))

(defn draw
  [region]
  (->> (for [i (range 15)
             j (range 15)]
         [i j])
       (partition 15)
       (map (fn [row]
              (map (fn [position]
                     (if (contains? region position)
                       "#"
                       "."))
                   row)))
       (map (fn [row]
              (apply str row)))
       (clojure.string/join "\n")
       (println)))

;(draw #{'(4 3) '(1 1) '(3 4) '(1 3) '(2 4) '(0 2) '(2 1) '(3 2)})

;(remove #{1 2 3} (list 3 5 6))
;(#{1 2 3} 1)
;(#{1 2 3} 5)

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 140)
           (is= (part-1 test-input-large) 1930))}
  [input]
  (let [garden-map (create-garden-map input)
        regions (find-regions garden-map)]
    (reduce (fn [a region]
              (+ a (* (count region) (get-perimeter-length region))))
            0
            regions)))

(defn get-side
  {:test (fn []
           (is= (get-side #{[0 0]
                            [0 1]
                            [0 2]
                            [0 3]}
                          [1 0]
                          [-1 0]
                          [[0 1] [0 -1]])
                #{[1 0] [1 1] [1 2] [1 3]})
           (is= (get-side (find-region (create-garden-map test-input-large)
                                       [5 2])
                          [5 3]
                          [0 -1]
                          [[1 0] [-1 0]])
                #{[5 3]}))}
  [region position region-direction directions-to-walk]
  (loop [side-points #{position}]
    (let [new-points (->> side-points
                          (map (fn [p]
                                 (->> directions-to-walk
                                      (map (fn [d]
                                             (map + d p)))
                                      (remove region)
                                      (filter (fn [p] (contains? region (map + p region-direction))))
                                      (remove side-points))))
                          (apply concat)
                          (into #{}))]
      (if (empty? new-points)
        side-points
        (recur (clojure.set/union side-points new-points))))))

;(comment
;  (frequencies (vals (frequencies (get-perimeter
;                                    (find-region (create-garden-map input) [13 72])))))
;  )

(def other-directions {[0 1]  [[1 0] [-1 0]]
                       [0 -1] [[1 0] [-1 0]]
                       [1 0]  [[0 1] [0 -1]]
                       [-1 0] [[0 1] [0 -1]]})

(defn get-number-of-sides
  {:test (fn []
           (is= (get-number-of-sides #{[0 0]
                                       [0 1]
                                       [0 2]
                                       [0 3]})
                4)
           (is= (get-number-of-sides #{[1 2]
                                       [2 2]
                                       [2 3]
                                       [3 3]})
                8)
           (is= (get-number-of-sides (find-region (create-garden-map test-input-large)
                                                  [5 2]))
                16))}
  [region]
  (let [boundary-points (into #{} (get-perimeter region))
        boundary-points-with-directions (reduce (fn [a p]
                                                  (assoc a p (->> directions
                                                                  (filter (fn [d] (contains? region (map + d p))))
                                                                  (into #{}))))
                                                {}
                                                boundary-points)]
    (loop [boundary-points-with-directions boundary-points-with-directions
           sides 0]
      (if (empty? boundary-points-with-directions)
        sides
        (let [[point directions] (first boundary-points-with-directions)
              direction (first directions)
              side-points (get-side region point direction (other-directions direction))]
          (recur (reduce (fn [a p]
                           (if (< 1 (count (get a p)))
                             (update a p disj direction)
                             (dissoc a p)))
                         boundary-points-with-directions
                         side-points)
                 (inc sides)))))))

;(draw (find-region (create-garden-map test-input-large)
;                   [5 2]))
;
;(draw (into #{} (get-perimeter (find-region (create-garden-map test-input-large)
;                                            [5 2]))))
;
;
;(draw (reduce clojure.set/union (get-number-of-sides (find-region (create-garden-map test-input-large)
;                                                                  [5 2]))))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input) 80)
           (is= (part-2 test-input-large) 1206))}
  [input]
  (let [garden-map (create-garden-map input)
        regions (find-regions garden-map)]
    (reduce (fn [a region]
              (+ a (* (count region) (get-number-of-sides region))))
            0
            regions)))

(comment
  ;; "Elapsed time: 637.255417 msecs"
  ;=> 1431316
  (time (part-1 input))

  ;; "Elapsed time: 724.939291 msecs"
  ;=> 821428
  (time (part-2 input))
  )
