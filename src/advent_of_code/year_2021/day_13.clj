(ns advent-of-code.year-2021.day-13)

(def input (slurp "src/advent_of_code/year_2021/inputs/day13.txt"))
(def input-dots (first (clojure.string/split input #"\n\n")))
(def input-folds (second (clojure.string/split input #"\n\n")))
(def test-input "6,10\n0,14\n9,10\n0,3\n10,4\n4,11\n6,0\n6,12\n4,1\n0,13\n10,12\n3,4\n3,0\n8,4\n1,10\n2,14\n8,10\n9,0\n\nfold along y=7\nfold along x=5")
(def test-input-dots (first (clojure.string/split test-input #"\n\n")))
(def test-input-folds (second (clojure.string/split test-input #"\n\n")))

(defn create-state
  [input-dots]
  (reduce (fn [state line]
            (conj state (map read-string (clojure.string/split line #","))))
          #{}
          (clojure.string/split-lines input-dots)))

(defn fold
  [state instruction]
  (let [is-up-fold (re-find #"y" instruction)
        fold-around (read-string (re-find #"\d+" instruction))]
    (reduce (fn [new-state [x y]]
              (if is-up-fold
                (if (< y fold-around)
                  (conj new-state [x y])
                  (conj new-state [x (- y (* 2 (- y fold-around)))]))
                (if (> x fold-around)
                  (conj new-state [(- x fold-around 1) y])
                  (conj new-state [(- fold-around x 1) y]))))
            #{}
            state)))

(defn solve-a
  []
  (count (fold (create-state input-dots) (first (clojure.string/split-lines input-folds)))))

(comment
  (solve-a)
  ; 810
  )

(defn print-state
  [state]
  (let [[max-x max-y] (reduce (fn [[max-x max-y] [x y]]
                                [(max max-x x) (max max-y y)])
                              [0 0]
                              state)
        initial-print-state (into [] (repeat (inc max-x) (into [] (repeat (inc max-y) " "))))
        print-state (reduce (fn [print-state [x y]]
                              (assoc-in print-state [x y] "#"))
                            initial-print-state
                            state)]
    (doseq [x (range (inc max-x))]
      (println (apply str (nth print-state x))))))

(defn solve-b
  []
  (-> (reduce fold (create-state input-dots) (clojure.string/split-lines input-folds))
      (print-state)))

(comment
  (solve-b)
  ; HLBUBGFR
  )
