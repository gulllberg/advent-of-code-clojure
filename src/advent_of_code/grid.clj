(ns advent-of-code.grid)

(defn parse-grid
  ([input parse-fn]
   (let [lines (into [] (clojure.string/split-lines input))]
                      (reduce-kv (fn [a i line]
                                   (reduce-kv (fn [a j c]
                                                (assoc a [i j] (parse-fn c)))
                                              a
                                              (into [] line)))
                                 {}
                                 lines)))
  ([input]
   (parse-grid input identity)))

(def directions-without-diagonals [[-1 0] [1 0] [0 -1] [0 1]])
(def directions-with-diagonals (for [x (range -1 2)
                                     y (range -1 2)
                                     :when (not= 0 x y)]
                                 [x y]))

(defn get-neighbours
  ([position directions]
   (map (fn [dir]
          (mapv + position dir))
        directions))
  ([position]
   (get-neighbours position directions-without-diagonals)))
