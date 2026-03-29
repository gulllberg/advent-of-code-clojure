(ns advent-of-code.year_2018.day_23
  (:require [ysera.test :refer [is= is is-not]]
            [advent-of-code.grid :refer [manhattan-distance-3d]]))

;; With Tomas

(def input (slurp "src/advent_of_code/year_2018/inputs/day23.txt"))
(def test-input "pos=<0,0,0>, r=4\npos=<1,0,0>, r=1\npos=<4,0,0>, r=3\npos=<0,2,0>, r=1\npos=<0,5,0>, r=3\npos=<0,0,3>, r=1\npos=<1,1,1>, r=1\npos=<1,1,2>, r=1\npos=<1,3,1>, r=1\n")
(def test-input-2 "pos=<10,12,12>, r=2\npos=<12,14,12>, r=2\npos=<16,12,12>, r=4\npos=<14,14,14>, r=6\npos=<50,50,50>, r=200\npos=<10,10,10>, r=5")

(defn parse-input
  [input]
  (->> (clojure.string/split-lines input)
       (map (fn [line]
              (let [[x y z r] (map read-string (re-seq #"-?\d+" line))]
                {:position [x y z] :radius r})))))

(defn in-range?
  {:test (fn []
           (is (in-range? [0 0 0] [1 0 0] 4))
           (is-not (in-range? [0 0 0] [0 2 0] 1)))}
  [position other-position radius]
  (<= (manhattan-distance-3d position other-position) radius))

(defn get-bots-in-range
  [position radius bots]
  (filter (fn [{other-position :position}]
            (in-range? position other-position radius))
          bots))

(defn get-bot-with-largest-radius
  [bots]
  (->> bots
       (sort-by :radius)
       (last)))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 7))}
  [input]
  (let [bots (parse-input input)
        bot-with-largest-radius (get-bot-with-largest-radius bots)]
    (-> (get-bots-in-range (:position bot-with-largest-radius)
                           (:radius bot-with-largest-radius)
                           bots)
        (count))))

(defn third [[_ _ z]] z)

(defn calculate-bots-in-range
  [bots position]
  (->> bots
       (filter (fn [n] (in-range? (:position n) position (:radius n))))
       (count)))

(defn find-better-position
  [bots position]
  (loop [distance 6
         factor 10000000
         start [position
                (calculate-bots-in-range bots position)
                (manhattan-distance-3d position [0 0 0])]]
    (let [next (loop [[position nanobots-in-range distance-to-origin :as current] start]
                 (println "[" factor "] looping with" position distance-to-origin nanobots-in-range)
                 (let [best (->> (for [x (range (- (first position) (* factor distance)) (+ (first position) (inc (* factor distance))) factor)
                                       y (range (- (second position) (* factor distance)) (+ (second position) (inc (* factor distance))) factor)
                                       z (range (- (third position) (* factor distance)) (+ (third position) (inc (* factor distance))) factor)]
                                   [x y z])
                                 (reduce (fn [a p] (conj a [p (calculate-bots-in-range bots p)]))
                                         [])
                                 (filter (fn [[_ n]] (>= n nanobots-in-range)))
                                 (map (fn [[p n]] [p n (manhattan-distance-3d p [0 0 0])]))
                                 (sort-by (juxt (comp - second) first))
                                 (first))]
                   (if (= best current)
                     best
                     (recur best))))]
      (if (= factor 1)
        next
        (recur distance (quot factor 10) next)))))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input-2) 36))}
  [input]
  (-> (parse-input input)
      (find-better-position [0 0 0])
      (last)))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 16.952 msecs"
  ;=> 602

  (time (part-2 input))
  ;; "Elapsed time: 6812.553541 msecs"
  ;=> 110620102
  )
