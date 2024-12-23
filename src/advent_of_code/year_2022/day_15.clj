(ns advent-of-code.year-2022.day-15
  (:require [ysera.test :refer [is= is is-not]]))

(def test-input "Sensor at x=2, y=18: closest beacon is at x=-2, y=15\nSensor at x=9, y=16: closest beacon is at x=10, y=16\nSensor at x=13, y=2: closest beacon is at x=15, y=3\nSensor at x=12, y=14: closest beacon is at x=10, y=16\nSensor at x=10, y=20: closest beacon is at x=10, y=16\nSensor at x=14, y=17: closest beacon is at x=10, y=16\nSensor at x=8, y=7: closest beacon is at x=2, y=10\nSensor at x=2, y=0: closest beacon is at x=2, y=10\nSensor at x=0, y=11: closest beacon is at x=2, y=10\nSensor at x=20, y=14: closest beacon is at x=25, y=17\nSensor at x=17, y=20: closest beacon is at x=21, y=22\nSensor at x=16, y=7: closest beacon is at x=15, y=3\nSensor at x=14, y=3: closest beacon is at x=15, y=3\nSensor at x=20, y=1: closest beacon is at x=15, y=3")
(def input (slurp "src/advent_of_code/year_2022/inputs/day15.txt"))

(defn parse-input
  [input]
  (->> (clojure.string/split-lines input)
       (map (fn [line]
              (->> (re-seq #"-?\d+" line)
                   (map read-string)
                   (partition 2))))))

(defn get-manhattan-distance
  [p1 p2]
  (+ (abs (- (first p1) (first p2)))
     (abs (- (second p1) (second p2)))))

(defn get-sensor-beacon-distances
  [readings]
  (reduce (fn [a [sensor-p beacon-p]]
            (assoc a sensor-p (get-manhattan-distance sensor-p beacon-p)))
          {}
          readings))

(defn possible?
  [sensor-beacon-distances position]
  (reduce (fn [_ [sensor-p distance]]
            (if (<= (get-manhattan-distance sensor-p position) distance)
              (reduced false)
              true))
          true
          sensor-beacon-distances))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input 10) 26))}
  [input row-to-check]
  (let [readings (parse-input input)
        beacon-positions (into #{} (map second readings))
        sensor-beacon-distances (get-sensor-beacon-distances readings)
        max-distance (apply max (vals sensor-beacon-distances))
        min-x (apply min (map first (keys sensor-beacon-distances)))
        max-x (apply max (map first (keys sensor-beacon-distances)))]
    (reduce (fn [a x]
              (let [position [x row-to-check]]
                (if (and (not (contains? beacon-positions position))
                         (not (possible? sensor-beacon-distances position)))
                  (inc a)
                  a)))
            0
            (range (- min-x max-distance) (+ max-x max-distance 1)))))

(defn valid-position?
  [[x y] max-coord]
  (and (>= x 0)
       (>= y 0)
       (<= x max-coord)
       (<= y max-coord)))

(defn get-perimeter-positions
  [sensor-position distance max-coord]
  (let [starting-position (mapv + sensor-position [distance 0])]
    (loop [positions (if (valid-position? starting-position max-coord) #{starting-position} #{})
           previous-position starting-position]
      (let [direction (cond
                        (and (> (first previous-position) (first sensor-position))
                             (>= (second previous-position) (second sensor-position)))
                        [-1 1]

                        (and (<= (first previous-position) (first sensor-position))
                             (> (second previous-position) (second sensor-position)))
                        [-1 -1]

                        (and (< (first previous-position) (first sensor-position))
                             (<= (second previous-position) (second sensor-position)))
                        [1 -1]

                        :else
                        [1 1])
            new-position (mapv + previous-position direction)]
        (if (= new-position starting-position)
          positions
          (recur (if (valid-position? new-position max-coord)
                   (conj positions new-position)
                   positions)
                 new-position))))))

(defn get-tuning-frequency
  [x y]
  (+ y (* x 4000000)))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input 20) 56000011))}
  [input max-coord]
  (let [readings (parse-input input)
        sensor-beacon-distances (get-sensor-beacon-distances readings)]
    (reduce (fn [_ [sensor-p distance]]
              (let [perimeter-positions (get-perimeter-positions sensor-p distance max-coord)]
                (if-let [result (reduce (fn [_ [x y]]
                                          (if (possible? sensor-beacon-distances [(inc x) y])
                                            (reduced [(inc x) y])
                                            nil))
                                        nil
                                        perimeter-positions)]
                  (reduced (apply get-tuning-frequency result))
                  nil)))
            nil
            sensor-beacon-distances)))

(comment
  (time (part-1 input 2000000))
  ; 4582667
  ; "Elapsed time: 10454.607125 msecs"

  (time (part-2 input 4000000))
  ; 10961118625406
  ; "Elapsed time: 28527.998125 msecs"
  )
