(ns advent-of-code.year-2023.day-17
  (:require [ysera.test :refer [is= is is-not]]
            [clojure.data.priority-map :refer [priority-map priority-map-keyfn]]
            [advent-of-code.grid :refer [manhattan-distance]]))

(def input (slurp "src/advent_of_code/year_2023/inputs/day17.txt"))
(def test-input "2413432311323\n3215453535623\n3255245654254\n3446585845452\n4546657867536\n1438598798454\n4457876987766\n3637877979653\n4654967986887\n4564679986453\n1224686865563\n2546548887735\n4322674655533")

(defn get-heat-loss-map
  [input]
  (let [lines (clojure.string/split-lines input)]
    (reduce (fn [a i]
              (let [line (nth lines i)]
                (reduce (fn [[a max-i max-j] j]
                          [(assoc a [i j] (read-string (str (nth line j)))) (max max-i i) (max max-j j)])
                        a
                        (range (count line)))))
            [{} 0 0]
            (range (count lines)))))

(defn get-neighbours
  [heat-loss-map {position :position direction :direction straight :straight}]
  (reduce (fn [a d]
            (let [pos (mapv + position d)]
              (if (contains? heat-loss-map pos)
                (conj a {:position pos :direction d :straight (if (= direction d) (inc straight) 1)})
                a)))
          #{}
          (remove (fn [d]
                    (or (and (= d direction) (= straight 3))
                        (= d (mapv * direction [-1 -1]))))
                  [[1 0] [-1 0] [0 1] [0 -1]])))

;; https://en.wikipedia.org/wiki/A*_search_algorithm
(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 102))}
  [input]
  (let [[heat-loss-map max-i max-j] (get-heat-loss-map input)
        end-position [max-i max-j]]
    (loop [unvisited (priority-map-keyfn first {:position [0 0] :direction [0 1] :straight 0} [(manhattan-distance [0 0] end-position) 0])
           visited {}]
      (let [[next-to-process [_ cost-so-far]] (peek unvisited)
            unvisited (pop unvisited)]
        (if (= (:position next-to-process) end-position)
          cost-so-far
          (let [neighbours (->> (get-neighbours heat-loss-map next-to-process)
                                (remove (fn [n]
                                          (contains? visited n))))]
            (recur (reduce (fn [unvisited neighbour]
                             (let [old-cost (get-in unvisited [neighbour 1] ##Inf)
                                   best-cost-less-straight (->> (range 1 (:straight neighbour))
                                                                (reduce (fn [a s]
                                                                          (min a (get-in  unvisited [(assoc neighbour :straight s) 1] ##Inf)))
                                                                        old-cost))
                                   new-cost (+ cost-so-far (get heat-loss-map (:position neighbour)))]
                               (if (< new-cost best-cost-less-straight)
                                 (assoc unvisited neighbour [(+ new-cost (manhattan-distance (:position neighbour) end-position)) new-cost])
                                 unvisited)))
                           unvisited
                           neighbours)
                   (assoc visited next-to-process cost-so-far))))))))

(defn get-neighbours-2
  [heat-loss-map {position :position direction :direction straight :straight}]
  (if (< straight 4)
    (let [new-position (mapv + position direction)]
      (if (contains? heat-loss-map new-position)
        #{{:position new-position :direction direction :straight (inc straight)}}
        #{}))
    (reduce (fn [a d]
              (let [pos (mapv + position d)]
                (if (contains? heat-loss-map pos)
                  (conj a {:position pos :direction d :straight (if (= direction d) (inc straight) 1)})
                  a)))
            #{}
            (remove (fn [d]
                      (or (and (= d direction) (= straight 10))
                          (= d (mapv * direction [-1 -1]))))
                    [[1 0] [-1 0] [0 1] [0 -1]]))))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input) 94)
           (is= (part-2 "111111111111\n999999999991\n999999999991\n999999999991\n999999999991") 71))}
  [input]
  (let [[heat-loss-map max-i max-j] (get-heat-loss-map input)
        end-position [max-i max-j]]
    (loop [unvisited (priority-map {:position [0 0] :direction [0 1] :straight 0} 0)
           visited {}]
      (let [[next-to-process cost-so-far] (peek unvisited)
            unvisited (pop unvisited)]
        (if (= (:position next-to-process) end-position)
          cost-so-far
          (let [neighbours (->> (get-neighbours-2 heat-loss-map next-to-process)
                                (remove (fn [n]
                                          (contains? visited n))))]
            (recur (reduce (fn [unvisited neighbour]
                             (let [old-cost (get unvisited neighbour ##Inf)
                                   best-cost-less-straight (->> (range 4 (:straight neighbour))
                                                                (reduce (fn [a s]
                                                                          (min a (get unvisited (assoc neighbour :straight s) ##Inf)))
                                                                        old-cost))
                                   new-cost (+ cost-so-far (get heat-loss-map (:position neighbour)))]
                               (if (and (< new-cost best-cost-less-straight)
                                        (or (not= end-position (:position neighbour))
                                            (>= (:straight neighbour) 4)))
                                 (assoc unvisited neighbour new-cost)
                                 unvisited)))
                           unvisited
                           neighbours)
                   (assoc visited next-to-process cost-so-far))))))))

(comment
  (time (part-1 input))
  ;; 1128
  ;; "Elapsed time: 1123.751709 msecs"

  (time (part-2 input))
  ;; 1268
  ;; "Elapsed time: 3205.003792 msecs"

  ;; Note: This day was refactored/improved in several steps (adding priority map and a-star, at first the run times were
  ;; Part 1 "Elapsed time: 180491.276504 msecs"
  ;; PArt 2 "Elapsed time: 1516349.454443 msecs"
  )
