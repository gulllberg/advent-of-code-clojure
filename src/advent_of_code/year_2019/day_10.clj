(ns advent-of-code.year-2019.day-10
  (:require [ysera.test :refer [is= is is-not]]
            [advent-of-code.grid :refer [parse-grid manhattan-distance]]
            [clojure.math :refer [atan2 PI]]))

(def input (slurp "src/advent_of_code/year_2019/inputs/day10.txt"))
(def test-input ".#..#\n.....\n#####\n....#\n...##")
(def test-input-2 "......#.#.\n#..#.#....\n..#######.\n.#.#.###..\n.#..#.....\n..#....#.#\n#..#....#.\n.##.#..###\n##...#..#.\n.#....####")
(def test-input-3 "#.#...#.#.\n.###....#.\n.#....#...\n##.#.#.#.#\n....#.#.#.\n.##..###.#\n..#...##..\n..##....##\n......#...\n.####.###.")
(def test-input-4 ".#..#..###\n####.###.#\n....###.#.\n..###.##.#\n##.##.#.#.\n....###..#\n..#.#..#.#\n#..#.#.###\n.##...##.#\n.....#.#..")
(def test-input-5 ".#..##.###...#######\n##.############..##.\n.#.######.########.#\n.###.#######.####.#.\n#####.##.#.##.###.##\n..#####..#.#########\n####################\n#.####....###.#.#.##\n##.#################\n#####.##.###..####..\n..######..##.#######\n####.##.####...##..#\n.#####..#.######.###\n##...#.##########...\n#.##########.#######\n.####.#.###.###.#.##\n....##.##.###..#####\n.#.#.###########.###\n#.#.#.#####.####.###\n###.##.####.##.#..##")
(def test-input-6 ".#....#####...#..\n##...##.#####..##\n##...#...#.#####.\n..#.....X...###..\n..#.#.....#....##")

(defn parse-input
  [input]
  (->> (parse-grid input)
       (reduce-kv (fn [a [i j] c]
                    (if (= c \#)
                      (conj a [j i])
                      a))
                  #{})))

(defn get-angle
  [[start-x start-y] [target-x target-y]]
  (let [delta-x (- target-x start-x)
        delta-y (- target-y start-y)
        ;; (atan2 x y) means 0 is north (up)
        ;; atan2 returns -pi to pi, convert to 0 to 2pi
        ;; In this coordinate system positive y is down, so invert it.
        angle-minus-pi-to-pi (atan2 delta-x (- delta-y))]
    (if (neg? angle-minus-pi-to-pi)
      (+ (* 2 PI) angle-minus-pi-to-pi)
      angle-minus-pi-to-pi)))

(defn group-by-angle
  [asteroids start]
  (reduce (fn [a asteroid]
            (let [angle (get-angle start asteroid)]
              (update a angle (fnil conj []) asteroid)))
          {}
          asteroids))

(defn find-best-asteroid
  [asteroids]
  (reduce (fn [[best _ :as a] start]
            (let [asteroid-groups (group-by-angle (disj asteroids start) start)
                  n (count (keys asteroid-groups))]
              (if (> n best)
                [n start]
                a)))
          [0 nil]
          asteroids))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 8)
           (is= (part-1 test-input-2) 33)
           (is= (part-1 test-input-3) 35)
           (is= (part-1 test-input-4) 41)
           (is= (part-1 test-input-5) 210))}
  [input]
  (let [asteroids (parse-input input)]
    (-> (find-best-asteroid asteroids)
        (first))))

(defn sort-groups-by-distance
  [asteroid-groups start]
  (reduce-kv (fn [a angle asteroids]
               (assoc a angle (sort-by (fn [asteroid]
                                         (manhattan-distance start asteroid))
                                       asteroids)))
             {}
             asteroid-groups))

;; From https://ask.clojure.org/index.php/12125/add-interleave-all-like-partition-all-is-to-partition
(defn interleave-all
  "Like interleave, but stops when the longest seq is done, instead of
   the shortest."
  {:copyright "Rich Hickey, since this is a modified version of interleave"}
  ([] ())
  ([c1] (lazy-seq c1))
  ([c1 c2]
   (lazy-seq
     (let [s1 (seq c1) s2 (seq c2)]
       (cond
         (and s1 s2)                                        ; there are elements left in both
         (cons (first s1) (cons (first s2)
                                (interleave-all (rest s1) (rest s2))))
         s1                                                 ; s2 is done
         s1
         s2                                                 ; s1 is done
         s2))))
  ([c1 c2 & colls]
   (lazy-seq
     (let [ss (filter identity (map seq (conj colls c2 c1)))]
       (concat (map first ss) (apply interleave-all (map rest ss)))))))

(defn get-order-of-vaporization
  {:test (fn []
           (let [order-of-vaporization (get-order-of-vaporization (parse-input test-input-6) [8 3])]
             (is= (nth order-of-vaporization 0) [8 1])
             (is= (nth order-of-vaporization 1) [9 0])
             (is= (nth order-of-vaporization 17) [4 4])
             (is= (nth order-of-vaporization 20) [0 2])
             (is= (nth order-of-vaporization 30) [8 0])
             (is= (nth order-of-vaporization 34) [13 3])
             (is= (nth order-of-vaporization 35) [14 3])))}
  [asteroids position]
  (let [asteroids-groups (-> (group-by-angle (disj asteroids position) position)
                             (sort-groups-by-distance position))]
    (->> (keys asteroids-groups)
         (sort)
         (map (fn [angle] (get asteroids-groups angle)))
         (apply interleave-all))))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input-5) 802))}
  [input]
  (let [asteroids (parse-input input)
        position (-> (find-best-asteroid asteroids)
                     (second))
        order-of-vaporization (get-order-of-vaporization asteroids position)
        [x y] (nth order-of-vaporization 199)]
    (+ (* 100 x) y)))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 36.386875 msecs"
  ;=> 276

  (time (part-2 input))
  ;; "Elapsed time: 30.604083 msecs"
  ;=> 1321
  )
