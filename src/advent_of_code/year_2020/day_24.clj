(ns advent-of-code.year-2020.day-24
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2020/inputs/day24.txt"))

(defn parse-line
  {:test (fn []
           (is= (parse-line "nwwswee") [0 0])
           (is= (parse-line "esew") [-1 1]))}
  [line]
  (first (reduce (fn [[coord depth] c]
                   (cond
                     (= depth \n) (if (= c \e) [(mapv + coord [1 1]) nil] [(mapv + coord [1 -1]) nil])
                     (= depth \s) (if (= c \e) [(mapv + coord [-1 1]) nil] [(mapv + coord [-1 -1]) nil])
                     :else (condp = c
                             \n [coord c]
                             \s [coord c]
                             \e [(mapv + coord [0 2]) nil]
                             \w [(mapv + coord [0 -2]) nil])))
                 [[0 0] nil]
                 line)))

(defn execute-instructions
  [lines]
  (reduce (fn [room line]
            (let [tile (parse-line line)]
              (if (= (get room tile) :black)
                (assoc room tile :white)
                (assoc room tile :black))))
          {}
          lines))

(defn count-black-tiles
  [room]
  (->> room
       (vals)
       (filter (fn [v] (= v :black)))
       (count)))

(defn solve-a
  []
  (->> (clojure.string/split-lines input)
       (execute-instructions)
       (count-black-tiles)))

(comment
  (solve-a)
  ; 228
  )

(defn get-neighbours
  [tile]
  (let [dirs [[1 1] [1 -1] [0 -2] [-1 -1] [-1 1] [0 2]]]
    (map (fn [dir]
           (mapv + tile dir))
         dirs)))

(defn count-black-neighbours
  [room tile]
  (reduce (fn [n neighbour]
            (if (= (get room neighbour) :black)
              (inc n)
              n))
          0
          (get-neighbours tile)))

(defn initiate-candidates
  [room]
  (assoc room :candidates (reduce (fn [candidates tile]
                                    (if (= (get room tile) :black)
                                      (clojure.set/union candidates (into #{} (get-neighbours tile)) #{tile})
                                      candidates))
                                  #{}
                                  (keys room))))

(defn living-art
  [room]
  (reduce (fn [new-room tile]
            (let [n-black-neighbours (count-black-neighbours room tile)
                  colour (get room tile :white)]
              (if (= colour :black)
                (if (or (= n-black-neighbours 1) (= n-black-neighbours 2))
                  new-room
                  (-> new-room
                      (assoc tile :white)
                      (update :candidates clojure.set/union (into #{} (get-neighbours tile)))))
                (if (= n-black-neighbours 2)
                  (-> new-room
                      (assoc tile :black)
                      (update :candidates clojure.set/union (into #{} (get-neighbours tile))))
                  new-room))))
          (assoc room :candidates #{})
          (:candidates room)))

(defn do-100-days
  [room]
  (loop [i 0
         room room]
    (if (= i 100)
      (dissoc room :candidates)
      (recur (inc i) (living-art room)))))

(defn solve-b
  []
  (->> (clojure.string/split-lines input)
       (execute-instructions)
       (initiate-candidates)
       (do-100-days)
       (count-black-tiles)))

(comment
  (solve-b)
  ; 3672
  )
