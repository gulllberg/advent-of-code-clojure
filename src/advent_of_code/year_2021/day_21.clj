(ns advent-of-code.year-2021.day-21
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2021/inputs/day21.txt"))
(def player-1-start (nth (map read-string (re-seq #"\d+" input)) 1))
(def player-2-start (nth (map read-string (re-seq #"\d+" input)) 3))

(defn get-new-position
  [starting-position dice-rolls]
  (loop [pos (apply + starting-position dice-rolls)]
    (if (< pos 11)
      pos
      (recur (- pos 10)))))

(defn deterministic-dice
  {:test (fn []
           (is= (deterministic-dice 4 8) 739785))}
  [p1-start p2-start]
  (loop [p1-position p1-start
         p1-score 0
         p2-position p2-start
         p2-score 0
         player-turn :p1
         n-dice-rolls 0
         ;; After 100 it should roll back to 1, but since positions wraps with mod 10 it does not matter if dice is 101 etc
         dice-rolls (drop 1 (range))]
    (cond
      (>= p1-score 1000) (* p2-score n-dice-rolls)
      (>= p2-score 1000) (* p1-score n-dice-rolls)
      (= player-turn :p1) (let [new-position (get-new-position p1-position (take 3 dice-rolls))]
                            (recur new-position
                                   (+ p1-score new-position)
                                   p2-position
                                   p2-score
                                   :p2
                                   (+ n-dice-rolls 3)
                                   (drop 3 dice-rolls)))
      :else (let [new-position (get-new-position p2-position (take 3 dice-rolls))]
              (recur p1-position
                     p1-score
                     new-position
                     (+ p2-score new-position)
                     :p1
                     (+ n-dice-rolls 3)
                     (drop 3 dice-rolls))))))

(defn solve-a
  []
  (deterministic-dice player-1-start player-2-start))

(comment
  (solve-a)
  ; 916083
  )

; Outcome - number of ways to do it (3 sides, 3 rolls -> 27 outcomes)
; 3 - 1
; 4 - 3
; 5 - 6
; 6 - 7
; 7 - 6
; 8 - 3
; 9 - 1

(defn get-new-positions
  [p1-position p2-position player-turn]
  (if (= player-turn :p1)
    (map (fn [d]
           (get-new-position p1-position [d]))
         (range 3 10))
    (map (fn [d]
           (get-new-position p2-position [d]))
         (range 3 10))))

(defn i->magnitude
  [i]
  (get {0 1
        1 3
        2 6
        3 7
        4 6
        5 3
        6 1} i))

(defn dirac-dice
  ; Slow to run
  ;{:test (fn []
  ;         (is= (apply max (dirac-dice 4 0 8 0 :p1 1)) 444356092776315))}
  [p1-position p1-score p2-position p2-score player-turn magnitude]
  (cond
    (>= p1-score 21) [magnitude 0]
    (>= p2-score 21) [0 magnitude]
    :else (let [new-positions (get-new-positions p1-position p2-position player-turn)]
            (reduce (fn [[p1-magnitude-sum p2-magnitude-sum] [p1-magnitude p2-magnitude]]
                      [(+ p1-magnitude-sum p1-magnitude) (+ p2-magnitude-sum p2-magnitude)])
                    [0 0]
                    (map-indexed (fn [i p]
                                   (if (= player-turn :p1)
                                     (dirac-dice p (+ p1-score p) p2-position p2-score :p2 (* (i->magnitude i) magnitude))
                                     (dirac-dice p1-position p1-score p (+ p2-score p) :p1 (* (i->magnitude i) magnitude))))
                                 new-positions)))))

(defn solve-b
  []
  (apply max (dirac-dice player-1-start 0 player-2-start 0 :p1 1)))

(comment
  ; Could possibly do this more efficiently with one loop keeping track of positions and scores as turns go back and forth.
  ; Then the outcomes that diverge and then join only takes up 1 space.
  (time (solve-b))
  ; "Elapsed time: 18963.576984 msecs"
  ; 49982165861983
  )
