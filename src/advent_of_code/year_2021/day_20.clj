(ns advent-of-code.year-2021.day-20
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2021/inputs/day20.txt"))
(def input-algorithm (first (clojure.string/split input #"\n\n")))
(def input-image (second (clojure.string/split input #"\n\n")))
(def test-algorithm "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#")
(def test-image "#..#.\n#....\n##..#\n..#..\n..###")

(defn create-image-state
  [image]
  (let [lines (clojure.string/split-lines image)]
    (reduce (fn [state i]
              (reduce (fn [state j]
                        (assoc state [i j] (str (nth (nth lines i) j))))
                      state
                      (range (count (nth lines i)))))
            {}
            (range (count lines)))))

;; https://stackoverflow.com/questions/5057047/how-to-do-exponentiation-in-clojure
(defn exp [x n]
  (reduce * (repeat n x)))

(defn binary-list->index
  {:test (fn []
           (is= (binary-list->index ["#" "." "#" "#" "."]) 22)
           (is= (binary-list->index ["." "#" "." "." "#"]) 9))}
  [binary-list]
  (reduce-kv (fn [a i v]
               (+ a (* (if (= v "#") 1 0) (exp 2 i))))
             0
             (vec (reverse binary-list))))

(defn get-output-pixel
  {:test (fn []
           (is= (get-output-pixel test-algorithm "." (create-image-state test-image) [2 2]) "#"))}
  [algorithm not-found-value image-state c]
  (let [positions (map (fn [d]
                         (mapv + c d))
                       [[-1 -1] [-1 0] [-1 1]
                        [0 -1] [0 0] [0 1]
                        [1 -1] [1 0] [1 1]])
        binary-list (map (fn [p]
                           (get image-state p not-found-value))
                         positions)
        index (binary-list->index binary-list)]
    (str (nth algorithm index))))

;; Fixme: x and y are mixed up (should be row col)
(defn get-image-boundaries
  [image-state]
  (reduce (fn [[x-min x-max y-min y-max] [x y]]
            [(min x-min x) (max x-max x) (min y-min y) (max y-max y)])
          [##Inf 0 ##Inf 0]
          (keys image-state)))

(defn enhance-image
  {:test (fn []
           (let [output-image (enhance-image test-algorithm "." (create-image-state test-image))]
             (is= (get output-image [-1 -1]) ".")
             (is= (get output-image [1 2]) "#")))}
  [algorithm not-found-value image-state]
  (let [[x-min x-max y-min y-max] (get-image-boundaries image-state)
        [x-min x-max y-min y-max] [(dec x-min) (inc x-max) (dec y-min) (inc y-max)]]
    (reduce (fn [output-image x]
              (reduce (fn [output-image y]
                        (assoc output-image [x y] (get-output-pixel algorithm not-found-value image-state [x y])))
                      output-image
                      (range y-min (inc y-max))))
            {}
            (range x-min (inc x-max)))))

(defn get-next-not-found-value
  {:test (fn []
           (is= (get-next-not-found-value test-algorithm ".") ".")
           (is= (get-next-not-found-value input-algorithm ".") "#")
           (is= (get-next-not-found-value input-algorithm "#") "."))}
  [algorithm not-found-value]
  (if (= not-found-value ".")
    (str (first algorithm))
    (str (last algorithm))))

(defn enhance-image-n-times
  [algorithm not-found-value image-state n]
  (loop [output-image image-state
         not-found-value not-found-value
         i 0]
    (if (= i n)
      output-image
      (recur (enhance-image algorithm not-found-value output-image)
             (get-next-not-found-value algorithm not-found-value)
             (inc i)))))

(defn count-light-pixels
  {:test (fn []
           (is= (count-light-pixels (enhance-image-n-times test-algorithm "." (create-image-state test-image) 2))
                35)
           (is= (count-light-pixels (enhance-image-n-times test-algorithm "." (create-image-state test-image) 50))
                3351))}
  [image-state]
  (->> image-state
       (vals)
       (filter (fn [pixel]
                 (= pixel "#")))
       (count)))

(defn solve-a
  []
  (count-light-pixels (enhance-image-n-times input-algorithm "." (create-image-state input-image) 2)))

(comment
  (solve-a)
  ; 5229
  )

(defn solve-b
  []
  (count-light-pixels (enhance-image-n-times input-algorithm "." (create-image-state input-image) 50)))

(comment
  (time (solve-b))
  ; "Elapsed time: 14478.145676 msecs"
  ; 17009
  )
