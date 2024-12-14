(ns advent-of-code.year_2018.day_06
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2018/inputs/day06.txt"))

(defn input->coordinates
  {:test (fn []
           (is= (input->coordinates "1, 1\n1, 6\n8, 3\n3, 4\n5, 5\n8, 9")
                [[1 1] [1 6] [8 3] [3 4] [5 5] [8 9]]))}
  [input]
  (as-> input $
        (clojure.string/split-lines $)
        (map (fn [coordinate-string]
               (->> (clojure.string/split coordinate-string #", ")
                    (map read-string)))
             $)))

(defn find-corners
  {:test (fn []
           (is= (find-corners (input->coordinates "1, 1\n1, 6\n8, 3\n3, 4\n5, 5\n8, 9"))
                [[1 1] [8 9]]))}
  [coordinates]
  (let [x-coordinates (map first coordinates)
        y-coordinates (map second coordinates)]
    [[(apply min x-coordinates) (apply min y-coordinates)] [(apply max x-coordinates) (apply max y-coordinates)]]))

(defn get-manhattan-distance
  {:test (fn []
           (is= (get-manhattan-distance [1 2] [3 4]) 4)
           (is= (get-manhattan-distance [3 4] [1 2]) 4))}
  [c1 c2]
  (+ (Math/abs (- (first c1) (first c2))) (Math/abs (- (second c1) (second c2)))))

(defn edgy?
  {:test (fn []
           (is (edgy? (find-corners (input->coordinates "1, 1\n1, 6\n8, 3\n3, 4\n5, 5\n8, 9"))
                      [1 4]))
           (is-not (edgy? (find-corners (input->coordinates "1, 1\n1, 6\n8, 3\n3, 4\n5, 5\n8, 9"))
                          [2 4])))}
  [corners coordinate]
  (or (= (first coordinate) (get-in corners [0 0]))
      (= (first coordinate) (get-in corners [1 0]))
      (= (second coordinate) (get-in corners [1 0]))
      (= (second coordinate) (get-in corners [1 1]))))

(defn part-1
  []
  (let [coordinates (input->coordinates input)
        corners (find-corners coordinates)]
    (apply max (vals (reduce (fn [result x-coordinate]
                               (reduce (fn [result y-coordinate]
                                         (let [[_ shortest-coordinate] (reduce (fn [[shortest-distance shortest-coordinate] coordinate]
                                                                                 (let [distance (get-manhattan-distance [x-coordinate y-coordinate] coordinate)]
                                                                                   (cond (= distance shortest-distance)
                                                                                         [shortest-distance nil]
                                                                                         (> shortest-distance distance)
                                                                                         [distance coordinate]
                                                                                         :else
                                                                                         [shortest-distance shortest-coordinate])))
                                                                               [1000000000 nil]
                                                                               coordinates)]
                                           (cond (nil? shortest-coordinate)
                                                 result
                                                 (edgy? corners [x-coordinate y-coordinate])
                                                 (assoc result shortest-coordinate -1000000000)
                                                 :else
                                                 (update result shortest-coordinate inc))))
                                       result
                                       (range (get-in corners [0 1]) (inc (get-in corners [1 1])))))
                             (reduce (fn [a v] (assoc a v 0)) {} coordinates)
                             (range (get-in corners [0 0]) (inc (get-in corners [1 0]))))))))

(comment
  (time (part-1))
  ;; 4829
  ;; "Elapsed time: 20610.067708 msecs"
  )

(defn part-2
  []
  (let [coordinates (input->coordinates input)
        corners (find-corners coordinates)]
    (reduce (fn [size x-coordinate]
              (reduce (fn [size y-coordinate]
                        (let [total-distance (reduce (fn [total-distance coordinate]
                                                       (+ total-distance (get-manhattan-distance [x-coordinate y-coordinate] coordinate)))
                                                     0
                                                     coordinates)]
                          (if (< total-distance 10000)
                            (inc size)
                            size)))
                      size
                      (range (get-in corners [0 1]) (inc (get-in corners [1 1])))))
            0
            (range (get-in corners [0 0]) (inc (get-in corners [1 0]))))))

(comment
  (time (part-2))
  ;; 46966
  ;; "Elapsed time: 20156.139542 msecs"
  )
