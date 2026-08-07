(ns advent-of-code.year-2019.day-20
  (:require [ysera.test :refer [is= is is-not]]
            [advent-of-code.grid :refer [parse-grid get-neighbours]]))

(def input (slurp "src/advent_of_code/year_2019/inputs/day20.txt"))
(def test-input "         A           \n         A           \n  #######.#########  \n  #######.........#  \n  #######.#######.#  \n  #######.#######.#  \n  #######.#######.#  \n  #####  B    ###.#  \nBC...##  C    ###.#  \n  ##.##       ###.#  \n  ##...DE  F  ###.#  \n  #####    G  ###.#  \n  #########.#####.#  \nDE..#######...###.#  \n  #.#########.###.#  \nFG..#########.....#  \n  ###########.#####  \n             Z       \n             Z       ")
(def test-input-2 "             Z L X W       C                 \n             Z P Q B       K                 \n  ###########.#.#.#.#######.###############  \n  #...#.......#.#.......#.#.......#.#.#...#  \n  ###.#.#.#.#.#.#.#.###.#.#.#######.#.#.###  \n  #.#...#.#.#...#.#.#...#...#...#.#.......#  \n  #.###.#######.###.###.#.###.###.#.#######  \n  #...#.......#.#...#...#.............#...#  \n  #.#########.#######.#.#######.#######.###  \n  #...#.#    F       R I       Z    #.#.#.#  \n  #.###.#    D       E C       H    #.#.#.#  \n  #.#...#                           #...#.#  \n  #.###.#                           #.###.#  \n  #.#....OA                       WB..#.#..ZH\n  #.###.#                           #.#.#.#  \nCJ......#                           #.....#  \n  #######                           #######  \n  #.#....CK                         #......IC\n  #.###.#                           #.###.#  \n  #.....#                           #...#.#  \n  ###.###                           #.#.#.#  \nXF....#.#                         RF..#.#.#  \n  #####.#                           #######  \n  #......CJ                       NM..#...#  \n  ###.#.#                           #.###.#  \nRE....#.#                           #......RF\n  ###.###        X   X       L      #.#.#.#  \n  #.....#        F   Q       P      #.#.#.#  \n  ###.###########.###.#######.#########.###  \n  #.....#...#.....#.......#...#.....#.#...#  \n  #####.#.###.#######.#######.###.###.#.#.#  \n  #.......#.......#.#.#.#.#...#...#...#.#.#  \n  #####.###.#####.#.#.#.#.###.###.#.###.###  \n  #.......#.....#.#...#...............#...#  \n  #############.#.#.###.###################  \n               A O F   N                     \n               A A D   M                     ")

(defn letter-c?
  {:test (fn []
           (is (letter-c? \A))
           (is-not (letter-c? \#)))}
  [c]
  ;; A = 65, Z = 90
  (<= 65 (int c) 90))

(defn extract-label-for-movement
  [grid position movement]
  (let [cs (map (fn [move]
                  (get grid (mapv + position move)))
                movement)]
    (when (every? letter-c? cs)
      (apply str cs))))

(defn extract-label
  [grid position]
  (some (fn [movement]
          (extract-label-for-movement grid position movement))
        ;; Left
        [[[0 -2]
          [0 -1]]
         ;; Up
         [[-2 0]
          [-1 0]]
         ;; Right
         [[0 1]
          [0 2]]
         ;; Down
         [[1 0]
          [2 0]]]))

(defn add-labels
  [{grid :grid}]
  (-> (reduce-kv (fn [a position c]
                   (cond
                     (not= c \.)
                     a

                     (extract-label grid position)
                     (let [label (extract-label grid position)]
                       (-> a
                           (update :label->positions update label (fnil conj #{}) position)
                           (update :position->label assoc position label)))

                     :else
                     a))
                 {:label->positions {}
                  :position->label  {}}
                 grid)
      (assoc :grid grid)))

(defn add-start-goal
  [{label->positions :label->positions :as all}]
  (merge all
         {:start (first (get label->positions "AA"))
          :goal  (first (get label->positions "ZZ"))}))

(defn parse-input
  [input]
  (-> {:grid (parse-grid input)}
      (add-labels)
      (add-start-goal)))

(defn get-portal-neighbour
  [{position->label :position->label label->positions :label->positions} position]
  (when (contains? position->label position)
    (as-> (get position->label position) $
          (get label->positions $)
          (disj $ position)
          (first $))))

(defn solve-maze
  [{grid :grid start :start goal :goal :as maze}]
  (loop [steps 0
         visited #{start}
         boundary #{start}]
    (if (contains? boundary goal)
      steps
      (let [[visited boundary] (reduce (fn [a position]
                                         (let [neighbours (get-neighbours position)
                                               portal-neighbour (get-portal-neighbour maze position)
                                               neighbours (if portal-neighbour
                                                            (conj neighbours portal-neighbour)
                                                            neighbours)]
                                           (reduce (fn [[visited boundary :as a] position]
                                                     (if (or (not= \. (get grid position))
                                                             (contains? visited position))
                                                       a
                                                       [(conj visited position) (conj boundary position)]))
                                                   a
                                                   neighbours)))
                                       [visited #{}]
                                       boundary)]
        (recur (inc steps)
               visited
               boundary)))))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 23))}
  [input]
  (-> (parse-input input)
      (solve-maze)))

(defn outer-label?
  [[row col] min-row max-row min-col max-col]
  (or (<= row (+ min-row 3))
      (<= col (+ min-col 3))
      (>= row (- max-row 3))
      (>= col (- max-col 3))))

(defn add-labels-2
  [{grid :grid}]
  (let [positions (keys grid)
        rows (map first positions)
        cols (map second positions)
        min-row (apply min rows)
        max-row (apply max rows)
        min-col (apply min cols)
        max-col (apply max cols)]
    (-> (reduce-kv (fn [a position c]
                     (cond
                       (not= c \.)
                       a

                       (extract-label grid position)
                       (let [label (extract-label grid position)
                             outer (outer-label? position min-row max-row min-col max-col)
                             label-key (if outer :outer-label->position :inner-label->position)
                             position-key (if outer :position->outer-label :position->inner-label)]
                         (-> a
                             (update label-key assoc label position)
                             (update position-key assoc position label)))

                       :else
                       a))
                   {:outer-label->position {}
                    :inner-label->position {}
                    :position->outer-label {}
                    :position->inner-label {}}
                   grid)
        (assoc :grid grid))))

(defn add-start-goal-2
  [{outer-label->position :outer-label->position :as maze}]
  (merge maze
         {:start (get outer-label->position "AA")
          :goal  (get outer-label->position "ZZ")}))

;; They are not needed since they can never be used.
(defn remove-start-goal-portals
  [{start :start goal :goal :as maze}]
  (-> maze
      (update :outer-label->position dissoc "AA" "ZZ")
      (update :position->outer-label dissoc start goal)))

(defn parse-input-2
  [input]
  (-> {:grid (parse-grid input)}
      (add-labels-2)
      (add-start-goal-2)
      (remove-start-goal-portals)))

(defn get-portal-neighbour-2
  [{outer-label->position :outer-label->position
    inner-label->position :inner-label->position
    position->outer-label :position->outer-label
    position->inner-label :position->inner-label} level position]
  (cond
    (and (contains? position->outer-label position)
         (not (zero? level)))
    [-1 (->> (get position->outer-label position)
             (get inner-label->position))]

    (contains? position->inner-label position)
    [1 (->> (get position->inner-label position)
            (get outer-label->position))]

    :else
    nil))

(defn solve-maze-2
  [{grid :grid start :start goal :goal :as maze}]
  (loop [steps 0
         visited {0 #{start}}
         boundary {0 #{start}}]
    (if (get-in boundary [0 goal])
      steps
      (let [[visited boundary] (reduce-kv (fn [a level positions]
                                            (reduce (fn [a position]
                                                      (let [neighbours (map (fn [neighbour]
                                                                              [0 neighbour])
                                                                            (get-neighbours position))
                                                            portal-neighbour (get-portal-neighbour-2 maze level position)
                                                            neighbours (if portal-neighbour
                                                                         (conj neighbours portal-neighbour)
                                                                         neighbours)]
                                                        (reduce (fn [[visited boundary :as a] [level-change position]]
                                                                  (let [level (+ level level-change)]
                                                                    (if (or (not= \. (get grid position))
                                                                            (get-in visited [level position]))
                                                                      a
                                                                      [(update visited level (fnil conj #{}) position)
                                                                       (update boundary level (fnil conj #{}) position)])))
                                                                a
                                                                neighbours)))
                                                    a
                                                    positions))
                                          [visited {}]
                                          boundary)]
        (recur (inc steps)
               visited
               boundary)))))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input-2) 396))}
  [input]
  (-> (parse-input-2 input)
      (solve-maze-2)))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 65.336166 msecs"
  ;=> 464

  (time (part-2 input))
  ;; "Elapsed time: 1234.322833 msecs"
  ;=> 5802
  )
