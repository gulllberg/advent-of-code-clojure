(ns advent-of-code.year-2023.day-25
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2023/inputs/day25.txt"))
(def test-input "jqt: rhn xhk nvd\nrsh: frs pzl lsr\nxhk: hfx\ncmg: qnr nvd lhk bvb\nrhn: xhk bvb hfx\nbvb: xhk hfx\npzl: lsr hfx nvd\nqnr: nvd\nntq: jqt hfx bvb xhk\nnvd: lhk\nlsr: lhk\nrzs: qnr cmg lsr rsh\nfrs: qnr lhk lsr")

(defn create-state
  [input]
  (let [lines (clojure.string/split-lines input)]
    (reduce (fn [a line]
              (let [[from & to] (re-seq #"\w+" line)]
                (reduce (fn [a t]
                          (update a t conj from))
                        (update a from concat to)
                        to)))
            {}
            lines)))

; Assumptions:
; 1. There are two clusters/islands with 3 "bridges" between them
; 2. Shortest path between two nodes in same cluster will never cross bridges - THIS TURNED OUT NOT TO BE TRUE
; 3. There are 4 "unique" paths between nodes in same cluster (unique=does not re-use edge for two different paths)

; Algorithm (not used):
; 1. Pick a start node
; 2. Pick an end node
; 3. Find the shortest path to end node
; 4. Repeat 3, but without visiting any edges visited in 3
; 5. Repeat 3, but also without visiting edges from 4
; 6. Again remove edges visited in 5. If there still exists a path between start and end they are on same island.
; 7a. If a path was found, pick a new end node and go again (all nodes visited in 3-5 will not be a correct end node)
; 7b. If no path is found, the 3 paths that were found will each include one of the bridges.
; 8. For each of the 3 paths, add one edge back until you can find a path between start and end. That edge is a bridge.

; Algorithm 2 (used)
; 1. Pick a start node for cluster 1
; 2. Pick an adjacent node
; 3. Try to find 4 unique paths (as above)
; 4a. If 4 exist, all visited nodes are part of cluster 1 - NOT TRUE since assumption 2 turned out to be false, you might cross. But to-node is in cluster 1
; 4b. if 4 do not exist, the adjacent node is in cluster 2
; 5. Repeat until no more adjacent nodes. All remaining nodes are in cluster 2.

(defn shortest-path
  [state from to forbidden-edges]
  (loop [paths #{{:node from :visited-nodes #{from} :visited-edges #{}}}
         all-visited-nodes #{from}]
    (if (empty? paths)
      nil
      (let [finished-path (some (fn [path]
                                  (when (= (:node path) to)
                                    path))
                                paths)]
        (if finished-path
          finished-path
          (let [[paths all-visited-nodes] (reduce (fn [a path]
                                                    (reduce (fn [[paths all-visited-nodes] node]
                                                              (if (or (contains? all-visited-nodes node)
                                                                      (contains? forbidden-edges #{node (:node path)}))
                                                                [paths all-visited-nodes]
                                                                [(conj paths {:node          node
                                                                              :visited-nodes (conj (:visited-nodes path) node)
                                                                              :visited-edges (conj (:visited-edges path) #{node (:node path)})})
                                                                 (conj all-visited-nodes node)]))
                                                            a
                                                            (get state (:node path))))
                                                  [#{} all-visited-nodes]
                                                  paths)]
            (recur paths all-visited-nodes)))))))

(defn find-4-shortest-paths
  [state from to]
  (reduce (fn [[visited-nodes forbidden-edges] _]
            (let [path (shortest-path state from to forbidden-edges)]
              (if-not path
                (reduced nil)
                [(clojure.set/union visited-nodes (:visited-nodes path))
                 (clojure.set/union forbidden-edges (:visited-edges path))])))
          [#{} #{}]
          (range 4)))

(defn get-from-to
  [state cluster1 cluster2]
  (some (fn [node]
          (and (not (contains? cluster1 node))
               (not (contains? cluster2 node))
               (some (fn [neighbour]
                       (when (contains? cluster1 neighbour)
                         ;; from = neighbour (in cluster1)
                         ;; to = node (outside cluster1)
                         [neighbour node]))
                     (get state node))))
        (keys state)))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 54))}
  [input]
  (let [state (create-state input)
        all-nodes (keys state)]
    (loop [cluster1 #{(first all-nodes)}
           cluster2 #{}]
      (if (= (count all-nodes)
             (+ (count cluster1) (count cluster2)))
        (* (count cluster1) (count cluster2))
        (let [from-to (get-from-to state cluster1 cluster2)]
          (if-not from-to
            (* (count cluster1) (- (count all-nodes) (count cluster1)))
            (let [[from to] from-to
                  maybe-4-shortest-paths (find-4-shortest-paths state from to)]
              (if-not maybe-4-shortest-paths
                (recur cluster1 (conj cluster2 to))
                (recur (conj cluster1 to) cluster2)))))))))

(comment
  (time (part-1 input))
  ;; 580800
  ;; "Elapsed time: 2971.056125 msecs"
  )

(def test-graph {0 [1 2 3]
                 1 [0 3]
                 2 [0 3]
                 3 [0 1 2]})
(defn contract
  {:test (fn []
           (is= (contract test-graph 1 3)
                {0 ["1-3" 2 "1-3"]
                 "1-3" [0 0 2]
                 2 [0 "1-3"]})
           (is= (contract test-graph 3 0)
                {1 ["3-0" "3-0"]
                 2 ["3-0" "3-0"]
                 "3-0" [1 2 1 2]}))}
  [graph u v]
  (let [u-edges (graph u)
        v-edges (graph v)
        contracted-edges (reduce (fn [a t]
                                   (if (or (= t u)
                                           (= t v))
                                     a
                                     (conj a t)))
                                 []
                                 (concat u-edges v-edges))
        ;; To be able to tell which vertices have been contracted together
        uv (str u "-" v)]
    (reduce (fn [a t]
              (update a t (fn [ns]
                            (map (fn [n]
                                   ;; Transform edges to u or v to uv instead (the contracted node)
                                   (if (or (= n u)
                                           (= n v))
                                     uv
                                     n))
                                 ns))))
            (assoc (dissoc graph u v) uv contracted-edges)
            ;; Since we are renaming u, we need to check all contracted edges, not just v-edges
            contracted-edges)))

(defn karger-min-cut
  [graph]
  (loop [current-graph graph]
    (if (= 2 (count current-graph))
      current-graph
      (let [u (rand-nth (keys current-graph))
            v (rand-nth (current-graph u))]
        (recur (contract current-graph u v))))))

(defn solve-with-karger
  {:test (fn []
           (is= 54 (solve-with-karger test-input)))}
  [input]
  (let [graph (create-state input)
        [i cut] (loop [i 1]
                  (let [cut (karger-min-cut graph)]
                    (if (= 3 (count (first (vals cut))))
                      [i cut]
                      (recur (inc i)))))]
    (println "n iters to get cut" i)
    (->> cut
          (keys)
          (map (fn [k] (count (clojure.string/split k #"-"))))
          (reduce *))))

(comment
  (time (solve-with-karger input))
  ; => 580800
  ; Time is stochastic, normal is a couple of seconds
  ; (With implementation that only looked at size of cut the runtime was maybe 1s on average)
  ; n iters to get cut 1
  ; "Elapsed time: 243.255833 msecs"
  ; n iters to get cut 27
  ; "Elapsed time: 6319.1715 msecs"
  )
