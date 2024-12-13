(ns advent-of-code.year-2021.day-15
  (:require [ysera.collections :refer [seq-contains? index-of]]
            [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2021/inputs/day15.txt"))
(def test-input "1163751742\n1381373672\n2136511328\n3694931569\n7463417111\n1319128137\n1359912421\n3125421639\n1293138521\n2311944581")

(defn create-risk-level-state
  [input]
  (let [lines (clojure.string/split-lines input)]
    (reduce (fn [state i]
              (let [line (nth lines i)]
                (reduce (fn [state j]
                          (assoc state [i j] (read-string (str (nth line j)))))
                        state
                        (range (count line)))))
            {}
            (range (count lines)))))

(defn create-big-risk-level-state
  {:test (fn []
           (is= (create-big-risk-level-state "8")
                (create-risk-level-state "89123\n91234\n12345\n23456\n34567")))}
  [input]
  (let [lines (clojure.string/split-lines input)
        num-rows (count lines)
        num-cols (count (first lines))
        small-state (create-risk-level-state input)]
    (reduce (fn [state i]
              (reduce (fn [state j]
                        (let [quot-i (quot i num-rows)
                              rem-i (rem i num-rows)
                              quot-j (quot j num-cols)
                              rem-j (rem j num-cols)
                              small-state-risk-level (get small-state [rem-i rem-j])
                              risk-level-uncapped (+ small-state-risk-level quot-i quot-j)
                              risk-level (if (> risk-level-uncapped 9)
                                           (- risk-level-uncapped 9)
                                           risk-level-uncapped)]
                          (assoc state [i j] risk-level)))
                      state
                      (range (* 5 num-cols))))
            {}
            (range (* 5 num-rows)))))

(defn get-big-end-pos
  {:test (fn []
           (is= (get-big-end-pos test-input) [49 49]))}
  [input]
  [(dec (* 5 (count (clojure.string/split-lines input)))) (dec (* 5 (count (first (clojure.string/split-lines input)))))])

(defn get-neighbours
  [risk-level-state [i j]]
  (filter (fn [c]
            (contains? risk-level-state c))
          [[(inc i) j] [(dec i) j] [i (inc j)] [i (dec j)]]))

(defn get-path-risk-level
  [risk-level-state path]
  (reduce (fn [risk c]
            (+ risk (get risk-level-state c)))
          0
          path))

(defn path-finished?
  [path end-pos]
  (let [c (first path)]
    (= c end-pos)))

(defn get-new-paths-and-visited
  {:test (fn []
           (is= (get-new-paths-and-visited (create-risk-level-state test-input) #{[0 0]} (list [0 0]))
                [[(list [1 0] [0 0]) (list [0 1] [0 0])] #{[0 0] [1 0] [0 1]}]))}
  [risk-level-state visited path]
  (let [c (first path)
        neighbours (get-neighbours risk-level-state c)]
    (reduce (fn [[new-paths new-visited] c]
              (if (contains? new-visited c)
                [new-paths new-visited]
                [(conj new-paths (conj path c)) (conj new-visited c)]))
            [[] visited]
            neighbours)))

;; https://stackoverflow.com/questions/8641305/find-index-of-an-element-matching-a-predicate-in-clojure/8642069
(defn find-first-index
  {:test (fn []
           (is= (find-first-index pos? [-1 0 99 100 101]) 2)
           (is= (find-first-index pos? [-1 -2 -3]) 3))}
  [pred coll]
  (or (first (keep-indexed #(when (pred %2) %1) coll)) (count coll)))

(defn insert-at-index
  {:test (fn []
           (is= (insert-at-index (list 1 2 3 4 5) 3 6) (list 1 2 3 6 4 5)))}
  [coll i x]
  (concat (take i coll) (list x) (drop i coll)))

(defn insert-paths-into-sorted-list
  [risk-level-state sorted-paths new-paths]
  (reduce (fn [sorted-paths path]
            (let [path-risk-level (get-path-risk-level risk-level-state path)
                  index-to-insert (if (empty? sorted-paths)
                                    0
                                    (find-first-index (fn [path]
                                                        (< path-risk-level (get-path-risk-level risk-level-state path)))
                                                      sorted-paths))]
              (insert-at-index sorted-paths index-to-insert path)))
          sorted-paths
          new-paths))

(defn get-safest-route-risk-level
  {:test (fn []
           (is= (get-safest-route-risk-level test-input false) 40)
           (is= (get-safest-route-risk-level test-input true) 315))}
  [input use-big-state]
  (let [risk-level-state (if use-big-state (create-big-risk-level-state input) (create-risk-level-state input))
        start-pos [0 0]
        start-pos-risk-level (get risk-level-state start-pos)
        end-pos (if use-big-state (get-big-end-pos input) [(dec (count (clojure.string/split-lines input))) (dec (count (first (clojure.string/split-lines input))))])]
    (loop [paths (list (list start-pos))
           visited #{start-pos}
           i 1]
      (when (= 0 (mod i 100))
        (println i (count paths)))
      (let [path (first paths)]
        (if (path-finished? path end-pos)
          (- (get-path-risk-level risk-level-state path) start-pos-risk-level)
          ;; get new candidate paths and insert them sorted into paths
          (let [[new-paths new-visited] (get-new-paths-and-visited risk-level-state visited path)]
            (recur (insert-paths-into-sorted-list risk-level-state (rest paths) new-paths) new-visited (inc i))))))))

(defn part-1
  []
  (get-safest-route-risk-level input false))

(comment
  (time (part-1))
  ; Elapsed time: 3993.875522 msecs" (1)
  ; Elapsed time: ~40000 msecs" (2)
  ; "Elapsed time: 20010.912602 msecs" (3)
  ; 755
  )

(defn part-2
  []
  (get-safest-route-risk-level input true))

(comment
  (time (part-2))
  ; "Elapsed time: 3.2201793502699E7 msecs" (lol, only 9 hours)
  ; 3016
  ; 1. Explored paths breadth first, and when getting to an already visited node stopped if the old path was faster. Resulted in a lot of active paths since if a new path was faster the old one was still allowed to continue. Solved part 1 pretty well.
  ; 2. Like 1, but removed the old path if a new was faster. But doing so was also very slow.
  ; 3. Only take the shortest current path and continue. Guaranteed that when arriving at a node for the first time it is the optimal path.
  ; Saving entire paths (and maybe the big lookup table for risk levels) is probably what makes this so slow in all versions...
  )
