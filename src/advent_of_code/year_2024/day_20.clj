(ns advent-of-code.year-2024.day-20
  (:require [ysera.test :refer [is= is is-not]]))

(def test-input
  ["###############"
   "#...#...#.....#"
   "#.#.#.#.#.###.#"
   "#S#...#.#.#...#"
   "#######.#.#.###"
   "#######.#.#...#"
   "#######.#.###.#"
   "###..E#...#...#"
   "###.#######.###"
   "#...###...#...#"
   "#.#####.#.###.#"
   "#.#...#.#.#...#"
   "#.#.#.#.#.#.###"
   "#...#...#...###"
   "###############"])

(def input (->> (slurp "src/advent_of_code/year_2024/inputs/day20.txt")
                (clojure.string/split-lines)))

(defn get-track
  [input]
  (->> input
       (reduce-kv (fn [a y row]
                    (->> (into [] row)
                         (reduce-kv (fn [a x c]
                                      (if (not= c \#)
                                        (conj a [x y])
                                        a))
                                    a)))
                  #{})))

(defn find-character-coordinates
  [input character]
  (->> input
       (reduce-kv (fn [a y row]
                    (let [result (->> (into [] row)
                                      (reduce-kv (fn [a x c] (if (= c character) (reduced [x y]) a))
                                                 a))]
                      (if result (reduced result) nil)))
                  nil)))

(def test-track (get-track test-input))
(def test-start-position (find-character-coordinates test-input \S))
(def test-end-position (find-character-coordinates test-input \E))

(def directions [[-1 0] [1 0] [0 1] [0 -1]])

(defn walk-a-step
  {:test (fn []
           (is= (walk-a-step test-track [3 7])
                #{[4 7] [3 8]}))}
  [track position]
  (->> directions
       (map (fn [d] (map + d position)))
       (filter (fn [np] (contains? track np)))
       (into #{})))

(defn get-distances-to-end
  {:test (fn []
           (is= (get (get-distances-to-end test-track test-end-position) test-start-position) 84))}
  [track end-position]
  (loop [current-positions #{end-position}
         visited {end-position 0}
         steps 0]
    (if (empty? current-positions)
      visited
      (let [next-positions (->> current-positions
                                (map (fn [cp] (walk-a-step track cp)))
                                (apply clojure.set/union)
                                (remove (fn [np] (contains? visited np)))
                                (into #{}))]
        (recur next-positions
               (->> next-positions
                    (reduce (fn [visited np]
                              (assoc visited np (inc steps)))
                            visited))
               (inc steps))))))

(defn get-cheat-moves
  [distance]
  (->> (for [y (range (- distance) (inc distance))
             x (range (- distance) (inc distance))
             :when (<= (+ (abs x) (abs y)) distance)]
         [x y])))

(defn how-many-cheats
  [distances-to-end track cheat-moves threshold]
  (->> track
       (map (fn [p]
              (let [current-distance (get distances-to-end p)]
                (->> cheat-moves
                     (map (fn [cm] [cm (map + p cm)]))
                     (filter (fn [[[cmx cmy] cp]] (when-let [cheat-distance (get distances-to-end cp)]
                                                    (<= cheat-distance (- current-distance threshold (+ (abs cmx) (abs cmy)))))))
                     (count)))))
       (reduce +)))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input 1) 44))}
  [input threshold]
  (let [track (get-track input)
        end-position (find-character-coordinates input \E)
        distances-to-end (get-distances-to-end track end-position)
        cheat-moves (get-cheat-moves 2)]
    (how-many-cheats distances-to-end track cheat-moves threshold)))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input 50) 285))}
  [input threshold]
  (let [track (get-track input)
        end-position (find-character-coordinates input \E)
        distances-to-end (get-distances-to-end track end-position)
        cheat-moves (get-cheat-moves 20)]
    (how-many-cheats distances-to-end track cheat-moves threshold)))

(comment
  (time (part-1 input 100))
  ;; "Elapsed time: 122.794542 msecs"
  ;=> 1358

  (time (part-2 input 100))
  ;; "Elapsed time: 3160.267334 msecs"
  ;=> 1005856
  )
