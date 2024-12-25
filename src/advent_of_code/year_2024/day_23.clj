(ns advent-of-code.year-2024.day-23
  (:require [ysera.test :refer [is= is is-not]]))

(def input (->> (slurp "src/advent_of_code/year_2024/inputs/day23.txt")
                (clojure.string/split-lines)
                (map (fn [x] (clojure.string/split x #"-")))))

(def test-input (->> ["kh-tc" "qp-kh" "de-cg" "ka-co" "yn-aq" "qp-ub" "cg-tb" "vc-aq" "tb-ka" "wh-tc" "yn-cg" "kh-ub" "ta-co" "de-co" "tc-td" "tb-wq" "wh-td" "ta-ka" "td-qp" "aq-cg" "wq-ub" "ub-vc" "de-ta" "wq-aq" "wq-vc" "wh-yn" "ka-de" "kh-ta" "co-tc" "wh-qp" "tb-vc" "td-yn"]
                     (map (fn [x] (clojure.string/split x #"-")))))

(defn create-state
  [input]
  (as-> input $
        (reduce (fn [a [c1 c2]]
                  (-> a
                      (update c1 conj c2)
                      (update c2 conj c1)))
                {}
                $)
        (update-vals $ (fn [v] (into #{} v)))))

(def test-state (create-state test-input))
(def state (create-state input))

(defn cluster-of-three
  {:test (fn []
           (is= (count (cluster-of-three test-state "ta")) 3))}
  [state c]
  (->> (for [c1 (get state c)
             c2 (get state c1)
             :when (contains? (get state c2) c)]
         #{c c1 c2})
       (into #{})))

(defn part-1
  {:test (fn []
           (is= (part-1 test-state) 7))}
  [state]
  (->> state
       (keys)
       (filter (fn [c] (clojure.string/starts-with? c "t")))
       (reduce (fn [a c]
                 (println c)
                 (let [clusters (cluster-of-three (:state a) c)]
                   (-> a
                       (update :state dissoc c)
                       (update :result (fn [result] (apply conj result clusters))))))
               {:state  state
                :result #{}})
       (:result)
       (count)))

(defn get-clusters
  [state cluster]
  (->> cluster
       (map (fn [c] (get state c)))
       (apply clojure.set/intersection)
       (map (fn [c] (conj cluster c)))))

(defn get-largest-cluster
  {:test (fn []
           (is= (get-largest-cluster test-state)
                #{"co" "de" "ka" "ta"}))}
  [state]
  (loop [clusters (->> state
                       (keys)
                       (map (fn [c] #{c}))
                       (into #{}))
         n 0]
    (println n (count clusters))
    (if (= 1 (count clusters))
      (first clusters)
      (recur (->> clusters
                  (map (fn [cluster] (get-clusters state cluster)))
                  (flatten)
                  (into #{}))
             (inc n)))))

(defn get-password
  [cluster]
  (->> cluster
       (sort)
       (clojure.string/join ",")))

(defn part-2
  {:test (fn []
           (is= (part-2 test-state) "co,de,ka,ta"))}
  [state]
  (-> state
      (get-largest-cluster)
      (get-password)))

(comment
  (time (part-1 state))
  ;; "Elapsed time: 12.121958 msecs"
  ;=> 1156

  (time (part-2 state))
  ;; "Elapsed time: 3242.65625 msecs"
  ;=> "bx,cx,dr,dx,is,jg,km,kt,li,lt,nh,uf,um"
  )