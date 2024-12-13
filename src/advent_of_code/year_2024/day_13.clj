(ns advent-of-code.year-2024.day-13
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2024/inputs/day13.txt"))
(def test-input "")

(def test-input1 ["Button A: X+94, Y+34"
                  "Button B: X+22, Y+67"
                  "Prize: X=8400, Y=5400"])

(def test-input2 ["Button A: X+26, Y+66"
                  "Button B: X+67, Y+21"
                  "Prize: X=12748, Y=12176"])

(def input (->> (slurp "src/advent_of_code/year_2024/inputs/day13.txt")
                (clojure.string/split-lines)
                (remove (fn [row] (= row "")))
                (partition 3)))

(defn create-equation-data
  {:test (fn []
           (is= (create-equation-data test-input1)
                {:ax 94 :ay 34
                 :bx 22 :by 67
                 :rx 8400 :ry 5400}))}
  [[a-stuff b-stuff r-stuff]]
  (let [[ax ay] (map read-string (re-seq #"\d+" a-stuff))
        [bx by] (map read-string (re-seq #"\d+" b-stuff))
        [rx ry] (map read-string (re-seq #"\d+" r-stuff))]
    {:ax ax :ay ay :bx bx :by by :rx rx :ry ry}))

(def test-equation-1 (create-equation-data test-input1))
(def test-equation-2 (create-equation-data test-input2))

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn should-we-even-try?
  {:test (fn []
           (is (should-we-even-try? test-equation-1))
           (is-not (should-we-even-try? test-equation-2)))}
  [{ax :ax bx :bx ay :ay by :by rx :rx ry :ry}]
  (and (let [cdx (gcd ax bx)]
         (zero? (rem rx cdx)))
       (let [cdy (gcd ay by)]
         (zero? (rem ry cdy)))))

(def equations (->> input
                    (map create-equation-data)
                    (filter should-we-even-try?)))

(defn check-equation
  {:test (fn []
           (is= (check-equation test-equation-1 80 40) :solution)
           (is= (check-equation test-equation-1 20 40) :too-low)
           (is= (check-equation test-equation-1 90 40) :too-high))}
  [{ax :ax bx :bx ay :ay by :by rx :rx ry :ry} a b]
  (let [actual-x (+ (* a ax) (* b bx))
        actual-y (+ (* a ay) (* b by))]
    (cond (or (> actual-x rx) (> actual-y ry))
          :too-high
          (or (< actual-x rx) (< actual-y ry))
          :too-low
          :else :solution)))

(defn find-solutions
  {:test (fn []
           (is= (find-solutions test-equation-1)
                [{:a 80 :b 40}]))}
  [equation]
  (loop [[b & rbs] (range 101)
         solutions []]
    (if-not b
      solutions
      (let [result (loop [[a & ras] (range 101)]
                     (if-not a
                       :too-low
                       (let [result (check-equation equation a b)]
                         (case result
                           :too-low (recur ras)
                           :solution a
                           :too-high :too-high))))]
        (if (number? result)
          (recur rbs (conj solutions {:a result :b b}))
          (recur rbs solutions))))))

(defn find-cheapest-solution-cost
  {:test (fn []
           (is= (find-cheapest-solution-cost [{:a 80 :b 40}
                                              {:a 60 :b 90}])
                270))}
  [solutions]
  (->> solutions
       (map (fn [s] (+ (* 3 (:a s)) (:b s))))
       (apply min)))
(comment
  (time (->> equations
             (map find-solutions)
             (remove empty?)
             (map find-cheapest-solution-cost)
             (reduce +)))
  ; 36571
  )
; part two

(defn gcd-with-steps
  {:test (fn []
           (is= (gcd-with-steps 15 56)
                (list [1 1 4 3] [2 3 11 4] [1 4 15 11] [3 11 56 15])))}
  [a b]
  (loop [steps (list)
         biggest (max a b)
         smallest (min a b)]
    (let [remainder (rem biggest smallest)
          factor (quot biggest smallest)]
      (if (zero? remainder)
        steps
        (recur (conj steps [factor remainder biggest smallest])
               smallest
               remainder)))))

(defn get-e-and-f
  {:test (fn []
           (is= (get-e-and-f 56 15)
                [-4 15]))}
  [a b]
  (let [gcd-steps (gcd-with-steps a b)
        [factor remainder biggest smallest] (first gcd-steps)
        left-hand-side remainder           ; gcd?
        ]
    ; [factor remainder biggest smallest]
    ; remainder = biggest - factor * smallest
    (loop [steps (rest gcd-steps)
           right-hand-side [biggest 1 factor smallest]]
      (if (empty? steps)
        ; nånting
        (let [[factor remainder biggest smallest] (first steps)]
          )
        ))))

(defn solve-with-elimination
  {:test (fn []
           (is= (solve-with-elimination {:ax 3 :bx 4 :ay 5 :by 6 :rx 10 :ry 14})
                [-2 4])
           (is= (solve-with-elimination (create-equation-data test-input1))
                [80 40]))}
  [{ax :ax bx :bx ay :ay by :by rx :rx ry :ry}]
  ; a * ax + b * bx = rx
  ; a * ay + b * by = ry
  (let [a (/ (- (* rx by) (* ry bx))
             (- (* ax by) (* ay bx)))
        b (/ (- rx (* a ax)) bx)]
    [a b]))

(defn linearly-dependant?
  [{ax :ax bx :bx ay :ay by :by}]
  (= (/ ax bx)
     (/ ay by)))

(def big-number 10000000000000)
(def equations-2 (->> input
                      (map create-equation-data)
                      (map (fn [equation]
                             (-> equation
                                 (update :rx + big-number)
                                 (update :ry + big-number))))
                      (filter should-we-even-try?)))

(comment
  ;part 1
  (time (->> equations
             (map (fn [equation]
                    (if (linearly-dependant? equation)
                      equation
                      (solve-with-elimination equation))))
             (filter (fn [[a b]]
                       (and (integer? a)
                            (pos? a)
                            (integer? b)
                            (pos? b))))
             (map (fn [[a b]] (+ (* 3 a) b)))
             (reduce +)))

  ; part 2
  (time (->> equations-2
             (map (fn [equation]
                    (if (linearly-dependant? equation)
                      equation
                      (solve-with-elimination equation))))
             (filter (fn [[a b]]
                       (and (integer? a)
                            (pos? a)
                            (integer? b)
                            (pos? b))))
             (map (fn [[a b]] (+ (* 3 a) b)))
             (reduce +)))

  )

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 42))}
  [input]
  42)

(defn part-2
  {:test (fn []
           (is= (part-2 test-input) 42))}
  [input]
  42)

(comment
  ;;
  (time (part-1 input))

  ;;
  (time (part-2 input))
  )
