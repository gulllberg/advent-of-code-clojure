(ns advent-of-code.year-2021.day-12
  (:require [ysera.collections :refer [seq-contains?]]
            [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2021/inputs/day12.txt"))
(def test-input "start-A\nstart-b\nA-c\nA-b\nb-d\nA-end\nb-end")

(defn add-connection
  [connections from to]
  (if (contains? connections from)
    (update connections from conj to)
    (assoc connections from [to])))

(defn create-connections
  [input]
  (reduce (fn [connections line]
            (let [[from to] (clojure.string/split line #"-")]
              (-> connections
                  (add-connection from to)
                  (add-connection to from))))
          {}
          (clojure.string/split-lines input)))

(defn lower-case?
  {:test (fn []
           (is (lower-case? "abc"))
           (is-not (lower-case? "ABC")))}
  [string]
  (re-matches #"[a-z]+" string))

(defn visited-small-cave-twice?
  [path]
  (let [only-lower-case-path (filter lower-case? path)]
    (not= (count only-lower-case-path) (count (set only-lower-case-path)))))

(defn get-valid-continuations
  [connections visited position can-visit-a-small-cave-twice]
  (if (= position "end")
    []
    (remove (fn [p]
              (if can-visit-a-small-cave-twice
                (or (= p "start")
                    (and (lower-case? p)
                         (seq-contains? visited p)
                         (visited-small-cave-twice? visited)))
                (and (lower-case? p)
                     (seq-contains? visited p))))
            (get connections position))))

(defn get-new-potential-paths
  [connections path can-visit-a-small-cave-twice]
  (map (fn [continuation]
         (conj path continuation))
       (get-valid-continuations connections path (first path) can-visit-a-small-cave-twice)))

(defn get-all-paths
  {:test (fn []
           (is= (count (get-all-paths (create-connections test-input))) 10)
           (is= (count (get-all-paths (create-connections test-input) true)) 36))}
  ([connections can-visit-a-small-cave-twice]
   (loop [potential-paths #{(list "start")}
          finished-paths #{}]
     (if (empty? potential-paths)
       finished-paths
       (let [path-to-continue (first potential-paths)
             new-potential-paths (get-new-potential-paths connections path-to-continue can-visit-a-small-cave-twice)]
         (if (= (first path-to-continue) "end")
           (recur (disj potential-paths path-to-continue) (conj finished-paths path-to-continue))
           (if (empty? new-potential-paths)
             (recur (disj potential-paths path-to-continue) finished-paths)
             (recur (disj (apply conj potential-paths new-potential-paths) path-to-continue) finished-paths)))))))
  ([connections]
   (get-all-paths connections false)))

(defn part-1
  []
  (count (get-all-paths (create-connections input))))

(comment
  (time (part-1))
  ; 5874
  ; "Elapsed time: 74.089334 msecs"
  )

(defn part-2
  []
  (count (get-all-paths (create-connections input) true)))

(comment
  (time (part-2))
  ; "Elapsed time: 6836.101561 msecs"
  ; 153592
  )
