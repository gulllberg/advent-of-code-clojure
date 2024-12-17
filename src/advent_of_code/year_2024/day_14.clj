(ns advent-of-code.year-2024.day_14
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2024/inputs/day14.txt"))
(def test-input "p=0,4 v=3,-3\np=6,3 v=-1,-3\np=10,3 v=-1,2\np=2,0 v=2,-1\np=0,0 v=1,3\np=3,0 v=-2,-2\np=7,6 v=-1,-3\np=3,0 v=-1,-2\np=9,3 v=2,3\np=7,3 v=-1,2\np=2,4 v=2,-3\np=9,5 v=-3,-3")

(defn parse-input
  [input]
  (->> input
       (re-seq #"[\d-]+")
       (map read-string)
       (partition 4)
       (map (fn [[x y vx vy]]
              [[x y] [vx vy]]))))

(defn create-state
  [input width height]
  {:width  width
   :height height
   :guards (parse-input input)})

(def test-state (create-state test-input 11 7))

(defn move-guard-n
  {:test (fn []
           (is= (move-guard-n [[0 4] [3 -3]] 1 11 7)
                [[3 1] [3 -3]])
           (is= (move-guard-n [[3 1] [3 -3]] 1 11 7)
                [[6 5] [3 -3]]))}
  [guard n width height]
  (let [[vx vy] (second guard)]
    (update guard 0 (fn [[x y]]
                      [(mod (+ x (* vx n)) width)
                       (mod (+ y (* vy n)) height)]))))

(defn n-seconds
  [state n]
  (update state :guards (fn [guards]
                          (map (fn [guard]
                                 (move-guard-n guard n (:width state) (:height state)))
                               guards))))

(defn get-quadrants
  {:test (fn []
           (is= (get-quadrants 11 7)
                [[[0 4] [0 2]]
                 [[6 10] [0 2]]
                 [[0 4] [4 6]]
                 [[6 10] [4 6]]]))}
  [width height]
  ; [[x-min x-max] [y-min y-max]]
  [[[0 (dec (/ (dec width) 2))] [0 (dec (/ (dec height) 2))]]
   [[(/ (inc width) 2) (dec width)] [0 (dec (/ (dec height) 2))]]
   [[0 (dec (/ (dec width) 2))] [(/ (inc height) 2) (dec height)]]
   [[(/ (inc width) 2) (dec width)] [(/ (inc height) 2) (dec height)]]])

(defn get-safety-factor
  [state]
  (->> (get-quadrants (:width state) (:height state))
       (map (fn [[[x-min x-max] [y-min y-max]]]
              (->> (:guards state)
                   (filter (fn [[[x y] _]]
                             (and (<= x-min x x-max)
                                  (<= y-min y y-max))))
                   (count))))
       (reduce *)))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input 11 7) 12))}
  [input width height]
  (-> (create-state input width height)
      (n-seconds 100)
      (get-safety-factor)))

(defn draw
  [state]
  (let [guard-positions (->> (:guards state)
                             (map first)
                             (into #{}))]
    (->> (for [y (range (:height state))
               x (range (:width state))]
           [x y])
         (partition (:width state))
         (map (fn [row]
                (map (fn [position]
                       (if (contains? guard-positions position)
                         "#"
                         "."))
                     row)))
         (map (fn [row]
                (apply str row)))
         (clojure.string/join "\n"))))

(def state (create-state input 101 103))

(defn part-2
  [state start-n steps max-n]
  (loop [state (n-seconds state start-n)
         n start-n
         result ""]
    (if (<= n max-n)
      (recur (n-seconds state steps)
             (+ n steps)
             (str result (if (= n start-n) "" "\n\n") (draw state)))
      (spit (str "src/advent_of_code/year_2024/day14b-result-" start-n "-" steps ".txt") result))))

(comment
  ;; "Elapsed time: 19.658833 msecs"
  ;=> 230436441
  (time (part-1 input 101 103))

  (draw (n-seconds test-state 100))
  (draw state)

  ;; 8270
  ; "Elapsed time: 1259.016458 msecs"
  (time (part-2 state 190 101 10000))
  (time (part-2 state 8200 10 8270))

  ; See visualisation at
  ; https://github.com/gulllberg/advent-of-code-visualisations

  )
