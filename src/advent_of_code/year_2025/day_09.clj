(ns advent-of-code.year-2025.day-09
  (:require [ysera.test :refer [is= is is-not]]))

;; With Tomas

(def input (slurp "src/advent_of_code/year_2025/inputs/day09.txt"))
(def test-input "7,1\n11,1\n11,7\n9,7\n9,5\n2,5\n2,3\n7,3")

(defn parse-input
  [input]
  (->> (clojure.string/split-lines input)
       (mapv (fn [r] (mapv read-string (re-seq #"\d+" r))))))

(defn naive-calculation-of-largest-area
  {:test (fn []
           (is= (naive-calculation-of-largest-area (parse-input test-input))
                50))}
  [coordinates]
  (loop [[coordinate & coordinates] coordinates
         result 0]
    (if (empty? coordinates)
      result
      (recur coordinates
             (reduce (fn [result c]
                       (max result
                            (->> (map - coordinate c)
                                 (map abs)
                                 (map inc)
                                 (apply *))))
                     result
                     coordinates)))))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 50))}
  [input]
  (naive-calculation-of-largest-area (parse-input input)))

(defn vertical?
  {:test (fn []
           (is (vertical? [[7 1] [7 3]]))
           (is-not (vertical? [[5 2] [8 2]])))}
  [[[x11 _] [x12 _]]]
  (= x11 x12))

(defn horizontal?
  {:test (fn []
           (is (horizontal? [[1 7] [3 7]]))
           (is-not (horizontal? [[2 5] [2 8]])))}
  [[[_ y11] [_ y12]]]
  (= y11 y12))

(defn lines-intersect?
  {:test (fn []
           (is (lines-intersect? [[7 1] [7 3]] [[5 2] [8 2]]))
           (is-not (lines-intersect? [[7 1] [7 3]] [[5 4] [8 4]]))
           (is-not (lines-intersect? [[7 1] [7 3]] [[0 2] [0 8]])))}
  [[[x11 y11] [x12 y12] :as l1] [[x21 y21] [x22 y22] :as l2]]
  (cond
    (and (vertical? l1) (vertical? l2))
    (and (= x11 x21)
         (or (<= y21 y11 y22)
             (<= y21 y12 y22)
             (<= y22 y11 y21)
             (<= y22 y12 y21)
             (<= y11 y21 y12)
             (<= y11 y22 y12)
             (<= y12 y21 y11)
             (<= y12 y22 y11)))

    (and (horizontal? l1) (horizontal? l2))
    (and (= y11 y21)
         (or (<= x21 x11 x22)
             (<= x21 x12 x22)
             (<= x22 x11 x21)
             (<= x22 x12 x21)
             (<= x11 x21 x12)
             (<= x11 x22 x12)
             (<= x12 x21 x11)
             (<= x12 x22 x11)))

    (vertical? l1)
    (and (or (<= x21 x11 x22)
             (<= x22 x11 x21))
         (or (<= y11 y21 y12)
             (<= y12 y21 y11)))

    :else
    (and (or (<= x11 x21 x12)
             (<= x12 x21 x11))
         (or (<= y21 y11 y22)
             (<= y22 y11 y21)))))

(defn get-direction
  {:test (fn []
           (is= (get-direction [7 1] [7 3])
                [0 1]))}
  [p1 p2]
  (->> (map - p2 p1)
       (map (fn [x]
              (cond (pos? x) 1
                    (neg? x) -1
                    :else 0)))))

(defn create-outer-boundary
  {:test (fn []
           (is= (create-outer-boundary (parse-input test-input) [6 0])
                [[[6 0] [12 0]]
                 [[12 0] [12 8]]
                 [[12 8] [8 8]]
                 [[8 8] [8 6]]
                 [[8 6] [1 6]]
                 [[1 6] [1 2]]
                 [[1 2] [6 2]]
                 [[6 2] [6 0]]]))}
  [corners starting-point]
  (->> (concat corners (take 2 corners))
       (partition 3 1)
       (reduce (fn [[outer-boundary current-point] [p1 p2 p3]]
                 (let [direction (get-direction p1 p2)
                       turn-direction (get-direction p2 p3)
                       next-point (cond
                                    (= direction [1 0])
                                    (if (= turn-direction [0 1])
                                      (if (> (second p1) (second current-point))
                                        (map + p2 [1 -1])
                                        (map + p2 [-1 1]))
                                      (if (> (second p1) (second current-point))
                                        (map + p2 [-1 -1])
                                        (map + p2 [1 1])))

                                    (= direction [-1 0])
                                    (if (= turn-direction [0 1])
                                      (if (> (second p1) (second current-point))
                                        (map + p2 [-1 -1])
                                        (map + p2 [1 1]))
                                      (if (> (second p1) (second current-point))
                                        (map + p2 [1 -1])
                                        (map + p2 [-1 1])))

                                    (= direction [0 1])
                                    (if (= turn-direction [1 0])
                                      (if (< (first p1) (first current-point))
                                        (map + p2 [1 -1])
                                        (map + p2 [-1 1]))
                                      (if (< (first p1) (first current-point))
                                        (map + p2 [1 1])
                                        (map + p2 [-1 -1])))

                                    :else
                                    (if (= turn-direction [1 0])
                                      (if (< (first p1) (first current-point))
                                        (map + p2 [1 1])
                                        (map + p2 [-1 -1]))
                                      (if (< (first p1) (first current-point))
                                        (map + p2 [1 -1])
                                        (map + p2 [-1 1]))))]
                   [(conj outer-boundary [current-point next-point]) next-point]))
               [[] starting-point])
       (first)))

(defn inside-boundary?
  {:test (fn []
           (is (inside-boundary? (create-outer-boundary (parse-input test-input) [6 0]) [7 1] [9 5])))}
  [outer-boundary p1 p2]
  (let [lines (partition 2 1 [p1 [(first p1) (second p2)] p2 [(first p2) (second p1)] p1])]
    (every? (fn [l1]
              (every? (fn [l2]
                        (not (lines-intersect? l1 l2)))
                      outer-boundary))
            lines)))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input [6 0]) 24))}
  [input starting-point]
  (let [corners (parse-input input)
        outer-boundary (create-outer-boundary corners starting-point)]
    (loop [[coordinate & coordinates] corners
           result 0]
      (if (empty? coordinates)
        result
        (recur coordinates
               (reduce (fn [result c]
                         (if (inside-boundary? outer-boundary coordinate c)
                           (max result
                                (->> (map - coordinate c)
                                     (map abs)
                                     (map inc)
                                     (apply *)))
                           result))
                       result
                       coordinates))))))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 79.347333 msecs"
  ;=> 4737096935

  (time (part-2 input [97580 50267]))
  ;; "Elapsed time: 29762.549416 msecs"
  ;=> 1644094530
  )
