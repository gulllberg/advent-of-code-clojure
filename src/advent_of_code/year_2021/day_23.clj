(ns advent-of-code.year-2021.day-23
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2021/inputs/day23.txt"))
(def test-input "#############\n#...........#\n###B#C#B#D###\n  #A#D#C#A#\n  #########")
(def extra "#D#C#B#A#\n#D#B#A#C#")

(comment
  ; part a: 12530 (pen and paper, hehe...)
  )

(defn create-state
  [input]
  (let [input-letters (map keyword (re-seq #"\w" input))
        extra-letters (map keyword (re-seq #"\w" extra))]
    {:hall-left-1        nil
     :hall-left-2        nil
     :hall-central-left  nil
     :hall-central       nil
     :hall-central-right nil
     :hall-right-1       nil
     :hall-right-2       nil
     :A                  (list (first input-letters) (first extra-letters) (nth extra-letters 4) (nth input-letters 4))
     :B                  (list (second input-letters) (second extra-letters) (nth extra-letters 5) (nth input-letters 5))
     :C                  (list (nth input-letters 2) (nth extra-letters 2) (nth extra-letters 6) (nth input-letters 6))
     :D                  (list (nth input-letters 3) (nth extra-letters 3) (nth extra-letters 7) (nth input-letters 7))}))

(defn hallway?
  {:test (fn []
           (is-not (hallway? :D))
           (is (hallway? :hall-right-2)))}
  [position]
  (not (contains? #{:A :B :C :D} position)))

(defn can-leave-burrow?
  {:test (fn []
           (is (can-leave-burrow? {:A (list :D :C :A)} :A))
           (is (can-leave-burrow? {:A (list :A :C :A)} :A))
           (is-not (can-leave-burrow? {:A (list :A)} :A)))}
  [state burrow]
  (let [top-of-burrow (first (burrow state))]
    (or (not= top-of-burrow burrow)
        (not= 0 (count (remove (fn [a]
                                 (= a burrow))
                               (burrow state)))))))

(defn can-enter-burrow?
  {:test (fn []
           (is-not (can-enter-burrow? {:A (list :A)} :A :C))
           (is-not (can-enter-burrow? {:A (list :A :C :A)} :A :A))
           (is (can-enter-burrow? {:A (list :A)} :A :A)))}
  [state burrow amphipod]
  (and (= burrow amphipod)
       (= 0 (count (remove (fn [a]
                             (= a burrow))
                           (burrow state))))))

(defn get-amphipod
  {:test (fn []
           (is= (get-amphipod {:hall-central-left :A :B (list :A) :A (list :A)} :hall-central-left) :A)
           (is= (get-amphipod {:hall-central-left nil} :hall-central-left) nil)
           (is= (get-amphipod {:A []} :A) nil))}
  [state position]
  (if (hallway? position)
    (get state position)
    (first (get state position))))

(defn get-valid-to-positions
  {:test (fn []
           (is= (get-valid-to-positions {:hall-central-left :A :B (list :A) :A (list :A)} :hall-central-left) [:A])
           (is= (get-valid-to-positions {:hall-central-left :A :B (list :A) :A (list :A :B)} :hall-central-left) [])
           (is= (get-valid-to-positions {:hall-left-2 :A :hall-central-right :A :A (list :A :B)} :A) [:hall-left-1 :hall-central-left :hall-central])
           (is= (get-valid-to-positions {:hall-left-2 :A :hall-central-right :A :A (list :A)} :A) [])
           (is= (get-valid-to-positions (create-state input) :A) [:hall-left-1 :hall-left-2 :hall-central-left :hall-central :hall-central-right :hall-right-1 :hall-right-2]))}
  [state from-position]

  (let [amphipod (get-amphipod state from-position)]
    (remove nil?
            (condp = from-position
              :hall-left-1 [(when (can-enter-burrow? state :A amphipod) :A)
                            (when (and (can-enter-burrow? state :B amphipod) (not (:hall-central-left state))) :B)
                            (when (and (can-enter-burrow? state :C amphipod) (not (:hall-central-left state)) (not (:hall-central state))) :C)
                            (when (and (can-enter-burrow? state :D amphipod) (not (:hall-central-left state)) (not (:hall-central state)) (not (:hall-central-right state))) :D)]
              :hall-left-2 [(when (and (not (:hall-left-1 state)) (can-enter-burrow? state :A amphipod)) :A)
                            (when (and (can-enter-burrow? state :B amphipod) (not (:hall-left-1 state)) (not (:hall-central-left state))) :B)
                            (when (and (can-enter-burrow? state :C amphipod) (not (:hall-left-1 state)) (not (:hall-central-left state)) (not (:hall-central state))) :C)
                            (when (and (can-enter-burrow? state :D amphipod) (not (:hall-left-1 state)) (not (:hall-central-left state)) (not (:hall-central state)) (not (:hall-central-right state))) :D)]
              :hall-central-left [(when (can-enter-burrow? state :A amphipod) :A)
                                  (when (can-enter-burrow? state :B amphipod) :B)
                                  (when (and (can-enter-burrow? state :C amphipod) (not (:hall-central state))) :C)
                                  (when (and (can-enter-burrow? state :D amphipod) (not (:hall-central state)) (not (:hall-central-right state))) :D)]
              :hall-central [(when (and (can-enter-burrow? state :A amphipod) (not (:hall-central-left state))) :A)
                             (when (can-enter-burrow? state :B amphipod) :B)
                             (when (can-enter-burrow? state :C amphipod) :C)
                             (when (and (can-enter-burrow? state :D amphipod) (not (:hall-central-right state))) :D)]
              :hall-central-right [(when (and (can-enter-burrow? state :A amphipod) (not (:hall-central-left state)) (not (:hall-central state))) :A)
                                   (when (and (can-enter-burrow? state :B amphipod) (not (:hall-central state))) :B)
                                   (when (can-enter-burrow? state :C amphipod) :C)
                                   (when (can-enter-burrow? state :D amphipod) :D)]
              :hall-right-1 [(when (and (can-enter-burrow? state :A amphipod) (not (:hall-central-right state)) (not (:hall-central state)) (not (:hall-central-left state))) :A)
                             (when (and (can-enter-burrow? state :B amphipod) (not (:hall-central-right state)) (not (:hall-central state))) :B)
                             (when (and (can-enter-burrow? state :C amphipod) (not (:hall-central-right state))) :C)
                             (when (can-enter-burrow? state :D amphipod) :D)]
              :hall-right-2 [(when (and (can-enter-burrow? state :A amphipod) (not (:hall-right-1 state)) (not (:hall-central-right state)) (not (:hall-central state)) (not (:hall-central-left state))) :A)
                             (when (and (can-enter-burrow? state :B amphipod) (not (:hall-right-1 state)) (not (:hall-central-right state)) (not (:hall-central state))) :B)
                             (when (and (can-enter-burrow? state :C amphipod) (not (:hall-right-1 state)) (not (:hall-central-right state))) :C)
                             (when (and (can-enter-burrow? state :D amphipod) (not (:hall-right-1 state))) :D)]

              :A (if (can-leave-burrow? state :A) [(when-not (:hall-left-1 state) :hall-left-1)
                                                   (when-not (or (:hall-left-1 state) (:hall-left-2 state)) :hall-left-2)
                                                   (when-not (:hall-central-left state) :hall-central-left)
                                                   (when-not (or (:hall-central-left state) (:hall-central state)) :hall-central)
                                                   (when-not (or (:hall-central-left state) (:hall-central state) (:hall-central-right state)) :hall-central-right)
                                                   (when-not (or (:hall-central-left state) (:hall-central state) (:hall-central-right state) (:hall-right-1 state)) :hall-right-1)
                                                   (when-not (or (:hall-central-left state) (:hall-central state) (:hall-central-right state) (:hall-right-1 state) (:hall-right-2 state)) :hall-right-2)]
                                                  [])

              :B (if (can-leave-burrow? state :B) [(when-not (or (:hall-central-left state) (:hall-left-1 state)) :hall-left-1)
                                                   (when-not (or (:hall-central-left state) (:hall-left-1 state) (:hall-left-2 state)) :hall-left-2)
                                                   (when-not (:hall-central-left state) :hall-central-left)
                                                   (when-not (:hall-central state) :hall-central)
                                                   (when-not (or (:hall-central state) (:hall-central-right state)) :hall-central-right)
                                                   (when-not (or (:hall-central state) (:hall-central-right state) (:hall-right-1 state)) :hall-right-1)
                                                   (when-not (or (:hall-central state) (:hall-central-right state) (:hall-right-1 state) (:hall-right-2 state)) :hall-right-2)]
                                                  [])

              :C (if (can-leave-burrow? state :C) [(when-not (or (:hall-central-left state) (:hall-central state) (:hall-left-1 state)) :hall-left-1)
                                                   (when-not (or (:hall-central-left state) (:hall-central state) (:hall-left-1 state) (:hall-left-2 state)) :hall-left-2)
                                                   (when-not (or (:hall-central state) (:hall-central-left state)) :hall-central-left)
                                                   (when-not (:hall-central state) :hall-central)
                                                   (when-not (:hall-central-right state) :hall-central-right)
                                                   (when-not (or (:hall-central-right state) (:hall-right-1 state)) :hall-right-1)
                                                   (when-not (or (:hall-central-right state) (:hall-right-1 state) (:hall-right-2 state)) :hall-right-2)]
                                                  [])

              :D (if (can-leave-burrow? state :D) [(when-not (or (:hall-central-left state) (:hall-central state) (:hall-central-right state) (:hall-left-1 state)) :hall-left-1)
                                                   (when-not (or (:hall-central-left state) (:hall-central state) (:hall-central-right state) (:hall-left-1 state) (:hall-left-2 state)) :hall-left-2)
                                                   (when-not (or (:hall-central state) (:hall-central-right state) (:hall-central-left state)) :hall-central-left)
                                                   (when-not (or (:hall-central-right state) (:hall-central state)) :hall-central)
                                                   (when-not (:hall-central-right state) :hall-central-right)
                                                   (when-not (:hall-right-1 state) :hall-right-1)
                                                   (when-not (or (:hall-right-1 state) (:hall-right-2 state)) :hall-right-2)]
                                                  [])))))

(defn get-distance
  {:test (fn []
           (is= (get-distance {:A (list :A :A)} :hall-left-1 :A) 3)
           (is= (get-distance {:A (list :A)} :A :hall-central-right) 9))}
  ([state from-position to-position]
   (if (hallway? from-position)
     (get-distance state to-position from-position true)
     (get-distance state from-position to-position false)))
  ([state from-position to-position started-in-hallway]
   (let [length-of-burrow (count (get state from-position))]
     (+ (condp = from-position
          :A (condp = to-position
               :hall-left-1 2
               :hall-left-2 3
               :hall-central-left 2
               :hall-central 4
               :hall-central-right 6
               :hall-right-1 8
               :hall-right-2 9)
          :B (condp = to-position
               :hall-left-1 4
               :hall-left-2 5
               :hall-central-left 2
               :hall-central 2
               :hall-central-right 4
               :hall-right-1 6
               :hall-right-2 7)
          :C (condp = to-position
               :hall-left-1 6
               :hall-left-2 7
               :hall-central-left 4
               :hall-central 2
               :hall-central-right 2
               :hall-right-1 4
               :hall-right-2 5)
          :D (condp = to-position
               :hall-left-1 8
               :hall-left-2 9
               :hall-central-left 6
               :hall-central 4
               :hall-central-right 2
               :hall-right-1 2
               :hall-right-2 3))
        (- (if started-in-hallway 3 4) length-of-burrow)))))

(defn get-cost
  {:test (fn []
           (is= (get-cost {:A (list :A :A) :hall-left-1 :A} :hall-left-1 :A) 3)
           (is= (get-cost {:D (list :D)} :D :hall-central-right) 5000))}
  [state from-position to-position]
  (let [amphipod (get-amphipod state from-position)
        distance (get-distance state from-position to-position)]
    (condp = amphipod
      :A distance
      :B (* 10 distance)
      :C (* 100 distance)
      :D (* 1000 distance))))

(defn finished?
  [state]
  (and (= (:A state) (list :A :A :A :A))
       (= (:B state) (list :B :B :B :B))
       (= (:C state) (list :C :C :C :C))
       (= (:D state) (list :D :D :D :D))))

(defn move
  {:test (fn []
           (is= (move {:A (list :A :B)} :A :hall-left-1) {:A (list :B) :hall-left-1 :A})
           (is= (move {:A (list :A) :hall-left-1 :A} :hall-left-1 :A) {:A (list :A :A) :hall-left-1 nil}))}
  [state from-position to-position]
  (let [amphipod (get-amphipod state from-position)]
    (if (hallway? from-position)
      (-> state
          (assoc from-position nil)
          (update to-position conj amphipod))
      (-> state
          (assoc to-position amphipod)
          (update from-position (fn [b]
                                  (drop 1 b)))))))

(defn get-from-position-candidates
  {:test (fn []
           (is= (get-from-position-candidates (create-state input)) [:A :B :C :D]))}
  [state]
  (remove (fn [from-position]
            (nil? (get-amphipod state from-position)))
          [:A :B :C :D :hall-left-1 :hall-left-2 :hall-central-left :hall-central :hall-central-right :hall-right-1 :hall-right-2]))

(defn find-solutions
  {:test (fn []
           (is= (->> test-input
                     (create-state)
                     (find-solutions)
                     (map :cost)
                     (apply min))
                44169))}
  [state]
  (loop [possible-solutions #{{:state state :cost 0}}
         finished-solutions #{}]
    (println (count possible-solutions) (count finished-solutions))
    (if (= 0 (count possible-solutions))
      finished-solutions
      (let [[new-possible new-finished] (reduce (fn [[possible finished] solution-to-continue]
                                                  (let [state (:state solution-to-continue)
                                                        cost (:cost solution-to-continue)]
                                                    (reduce (fn [[possible finished] from-position]
                                                              (reduce (fn [[possible finished] to-position]
                                                                        (let [move-cost (get-cost state from-position to-position)
                                                                              new-state (move state from-position to-position)]
                                                                          (if (finished? new-state)
                                                                            [possible (conj finished {:state new-state :cost (+ cost move-cost)})]
                                                                            [(conj possible {:state new-state :cost (+ cost move-cost)}) finished])))
                                                                      [possible finished]
                                                                      (get-valid-to-positions state from-position)))
                                                            [possible finished]
                                                            (get-from-position-candidates state))))
                                                [#{} finished-solutions]
                                                possible-solutions)]
        (recur new-possible new-finished)))))

(defn solve-b
  []
  (->> input
       (create-state)
       (find-solutions)
       (map :cost)
       (apply min)))

(comment
  (time (solve-b))
  ; "Elapsed time: 4450.787652 msecs"
  ; 50492

  ; Solution moves
  ; [:C :hall-right-2]
  ; [:B :hall-right-1]
  ; [:C :hall-left-2]
  ; [:C :hall-central-right]
  ; [:C :hall-left-1]
  ; [:B :hall-central]
  ; [:hall-central :C]
  ; [:A :hall-central]
  ; [:hall-central :C]
  ; [:B :hall-central-left]
  ; [:B :hall-central]
  ; [:hall-central-left :B]
  ; [:hall-left-1 :B]
  ; [:hall-left-2 :B]
  ; [:A :hall-left-2]
  ; [:A :hall-left-1]
  ; [:A :hall-central-left]
  ; [:hall-central-left :B]
  ; [:hall-central :A]
  ; [:hall-central-right :A]
  ; [:D :hall-central-left]
  ; [:hall-central-left :A]
  ; [:D :hall-central-right]
  ; [:hall-central-right :A]
  ; [:D :hall-central]
  ; [:hall-central :C]
  ; [:D :hall-central-right]
  ; [:hall-right-1 :D]
  ; [:hall-central-right :C]
  ; [:hall-left-1 :D]
  ; [:hall-left-2 :D]
  ; [:hall-right-2 :D]
  )
