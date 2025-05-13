(ns advent-of-code.year-2015.day-18
  (:require [ysera.test :refer [is= is is-not]]
            [advent-of-code.grid :as g]))

(def input (slurp "src/advent_of_code/year_2015/inputs/day18.txt"))
(def test-input ".#.#.#\n...##.\n#....#\n..#...\n#.#..#\n####..")

(defn parse-input
  [input]
  (g/parse-grid input (fn [c]
                        (= c \#))))

(defn next-generation
  [grid]
  (reduce-kv (fn [a p on]
               (let [number-of-neighbours (->> (g/get-neighbours p g/directions-with-diagonals)
                                               (map (fn [p] (get grid p)))
                                               (filter true?)
                                               (count))]
                 (cond (= 3 number-of-neighbours) (assoc a p true)
                       (and on (= 2 number-of-neighbours)) (assoc a p true)
                       :else (assoc a p false))))
             {}
             grid))

(defn n-generations
  {:test (fn []
           (is= (->> (n-generations (parse-input test-input) 4)
                     (vals)
                     (filter true?)
                     (count))
                4))}
  [grid n]
  (nth (iterate next-generation grid) n))

(defn part-1
  [input]
  (->> (n-generations (parse-input input) 100)
       (vals)
       (filter true?)
       (count)))

(defn get-corners
  [input]
  (let [size (count (clojure.string/split-lines input))]
    (into #{} (for [x [0 (dec size)]
                    y [0 (dec size)]]
                [x y]))))

(defn turn-on-corners
  [grid corners]
  (reduce (fn [a p]
            (assoc a p true))
          grid
          corners))

(defn next-generation-2
  [grid corners]
  (reduce-kv (fn [a p on]
               (if (contains? corners p)
                 (assoc a p true)
                 (let [number-of-neighbours (->> (g/get-neighbours p g/directions-with-diagonals)
                                                 (map (fn [p] (get grid p)))
                                                 (filter true?)
                                                 (count))]
                   (cond (= 3 number-of-neighbours) (assoc a p true)
                         (and on (= 2 number-of-neighbours)) (assoc a p true)
                         :else (assoc a p false)))))
             {}
             grid))

(defn n-generations-2
  {:test (fn []
           (let [corners (get-corners test-input)
                 grid (turn-on-corners (parse-input test-input) corners)]
             (is= (->> (n-generations-2 grid corners 5)
                       (vals)
                       (filter true?)
                       (count))
                  17)))}
  [grid corners n]
  (nth (iterate (fn [grid] (next-generation-2 grid corners)) grid) n))

(defn part-2
  [input]
  (let [corners (get-corners input)
        grid (turn-on-corners (parse-input input) corners)]
    (->> (n-generations-2 grid corners 100)
         (vals)
         (filter true?)
         (count))))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 3984.45825 msecs"
  ;=> 768

  (time (part-2 input))
  ;; "Elapsed time: 3682.30175 msecs"
  ;=> 781
  )

(defn get-neighbours-in-grid
  [cell size]
  (->> (g/get-neighbours cell g/directions-with-diagonals)
       (filter (fn [[x y]]
                 (and (>= x 0)
                      (>= y 0)
                      (<= x (dec size))
                      (<= y (dec size)))))))

(defn next-generation-sparse
  [live-cells size]
  (let [neighbors (reduce (fn [counts cell]
                            (reduce (fn [counts neighbor]
                                      (update counts neighbor (fnil inc 0)))
                                    counts
                                    (get-neighbours-in-grid cell size)))
                          {}
                          live-cells)]
    (into #{}
          (keep (fn [[cell count]]
                  (if (or (= 3 count)
                          (and (live-cells cell) (= 2 count)))
                    cell
                    nil))
                neighbors))))

(defn n-generations-sparse
  {:test (fn []
           (let [grid (parse-input test-input)
                 live-cells (->> (keys grid)
                                 (filter (fn [k]
                                           (get grid k)))
                                 (into #{}))]
             (is= (->> (n-generations-sparse live-cells 6 4)
                       (count))
                  4)))}
  [live-cells size n]
  (nth (iterate (fn [live-cells] (next-generation-sparse live-cells size)) live-cells) n))

(defn part-1-sparse
  [input]
  (let [grid (parse-input input)
        live-cells (->> (keys grid)
                        (filter (fn [k]
                                  (get grid k)))
                        (into #{}))]
    (->> (n-generations-sparse live-cells 100 100)
         (count))))

(comment
  (time (part-1-sparse input))
  ;; "Elapsed time: 804.19525 msecs"
  ;=> 768
  )