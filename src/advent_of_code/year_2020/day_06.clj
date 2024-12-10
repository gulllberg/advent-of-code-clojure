(ns advent-of-code.year-2020.day-06)

(def input (slurp "src/advent_of_code/year_2020/inputs/day06.txt"))

(defn solve-a
  []
  (reduce (fn [sum group]
            (-> group
                (clojure.string/replace #"\s" "")
                (clojure.string/split #"")
                (set)
                (count)
                (+ sum)))
          0
          (clojure.string/split input #"\n\n")))

(comment
  (solve-a)
  ; 6587
  )

(defn solve-b
  []
  (reduce (fn [sum group]
            (->> group
                 (clojure.string/split-lines)
                 ; Method 3
                 (map (fn [line]
                        (-> line
                            (clojure.string/split #"")
                            (set))))

                 ; Method 2
                 ;(map (comp set (partial map identity)))

                 ; Method 1
                 ;(map (fn [line]
                 ;       (->> line
                 ;            (map identity)
                 ;            (set))))
                 (apply clojure.set/intersection)           ; Change this to union to solve a instead :)
                 (count)
                 (+ sum)))
          0
          (clojure.string/split input #"\n\n")))

(comment
  (solve-b)
  ; 3235
  )
