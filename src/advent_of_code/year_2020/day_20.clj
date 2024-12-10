(ns advent-of-code.year-2020.day-20
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2020/inputs/day20.txt"))

(defn parse-tile
  [tile]
  (let [lines (clojure.string/split-lines tile)
        id (re-find #"\d+" (first lines))]
    [id (second lines) (last lines) (apply str (map first (rest lines))) (apply str (map last (rest lines)))]))

(defn create-tile-state
  [tiles]
  (reduce (fn [state tile]
            (let [parsed-tile (parse-tile tile)]
              (assoc state (first parsed-tile) (rest parsed-tile))))
          {}
          tiles))

(defn check-if-possible-neighbours
  [tile-1-edges tile-2-edges]
  (some (fn [[e-1 e-2]]
          (= e-1 e-2))
        (for [e-1 [(first tile-1-edges) (second tile-1-edges) (nth tile-1-edges 2) (nth tile-1-edges 3)
                   (apply str (reverse (first tile-1-edges))) (apply str (reverse (second tile-1-edges))) (apply str (reverse (nth tile-1-edges 2))) (apply str (reverse (nth tile-1-edges 3)))]
              e-2 [(first tile-2-edges) (second tile-2-edges) (nth tile-2-edges 2) (nth tile-2-edges 3)
                   (apply str (reverse (first tile-2-edges))) (apply str (reverse (second tile-2-edges))) (apply str (reverse (nth tile-2-edges 2))) (apply str (reverse (nth tile-2-edges 3)))]]
          [e-1 e-2])))

(defn count-possible-neighbours
  [tile-state]
  (let [ids (keys tile-state)]
    (loop [i 0
           j 1
           count-state (reduce (fn [s id]
                                 (assoc s id 0))
                               {}
                               ids)]
      (cond
        (= i (dec (count ids)))
        count-state

        (= j (count ids))
        (recur (inc i) (+ i 2) count-state)

        :else (if (check-if-possible-neighbours (get tile-state (nth ids i)) (get tile-state (nth ids j)))
                (recur i (inc j) (-> count-state
                                     (update (nth ids i) inc)
                                     (update (nth ids j) inc)))
                (recur i (inc j) count-state))))))

(defn solve-a
  []
  (let [count-state (count-possible-neighbours (create-tile-state (clojure.string/split input #"\n\n")))]
    (apply * (map read-string (keys (filter (fn [[k v]]
                                              (= v 2))
                                            count-state))))))

(comment
  (solve-a)
  ; 18449208814679
  )

(defn parse-input
  [input]
  (reduce (fn [state tile]
            (let [lines (clojure.string/split-lines tile)
                  id (re-find #"\d+" (first lines))]
              (assoc state id (rest lines))))
          {}
          (clojure.string/split input #"\n\n")))

(defn rotate-tile
  [tile i]
  (condp = i
    ; Not rotated
    0 tile

    ; Around y axis
    1 (map (fn [r] (apply str (reverse r))) tile)

    ; Around x axis
    2 (reverse tile)

    ; Around x and y axes
    3 (reverse (rotate-tile tile 1))

    ; 90 degrees left and around x axis (transposed)
    4 (map (fn [r] (apply str r)) (apply map list tile))

    ; 90 degrees right and around x axis
    5 (-> tile (rotate-tile 4) (rotate-tile 1))

    ; 90 degrees left
    6 (-> tile (rotate-tile 4) (rotate-tile 2))

    ; 90 degrees right
    7 (-> tile (rotate-tile 5) (rotate-tile 2))))

(defn get-rotated-tile-if-match
  [tile edge]
  (loop [i 0]
    (if (= i 8)
      nil
      (let [rotated-tile (rotate-tile tile i)]
        ; Matches left edge on tile
        (if (= (apply str (map first rotated-tile)) edge)
          rotated-tile
          (recur (inc i)))))))

(defn get-matching-tile
  ; Actual:		 [2579 (##..###### #..#..#... ..##....## #........# ........#. .........# ##.......# #........# .......... .#...#..##)]
  ; Expected:	 [2579 (##..###### #..#..#... ..##....## #........# ........#. .........# ##.......# #........# .......... .#...#..##)]
  ;{:test (fn []
  ;         (is= (get-matching-tile (dissoc (parse-input input) "2111") "##.#..##..")
  ;              ["2579" '("##..######"
  ;                         "#..#..#..."
  ;                         "..##....##"
  ;                         "#........#"
  ;                         "........#."
  ;                         ".........#"
  ;                         "##.......#
  ;                          #........#
  ;                           .........."
  ;                         ".#...#..##")]))}
  [tile-state edge]
  (let [ids (keys tile-state)]
    ; Always a match, so no need to check if i out of bounds
    (loop [i 0]
      (if-let [matching-tile (get-rotated-tile-if-match (get tile-state (nth ids i)) edge)]
        [(nth ids i) matching-tile]
        (recur (inc i))))))

(defn combine-full-picture
  [image-state]
  (reduce (fn [image tile-row-number]
            (concat image (map (fn [i]
                                 (reduce (fn [row tile-col-number]
                                           (str row (subs (nth (get image-state [tile-row-number tile-col-number]) i) 1 9)))
                                         ""
                                         (range 12)))
                               (range 1 9))))
          []
          (range 12)))

(defn count-sea-monsters
  [picture]
  ;                   #
  ; #    ##    ##    ###
  ;  #  #  #  #  #  #
  ; Sea monster is in 3x20 rectangle
  (loop [i 0
         j 0
         count 0]
    (cond
      ; No need to check last two rows
      (= i (- 96 2)) count

      ; No need to check last 19 columns
      (= j (- 96 19)) (recur (inc i) 0 count)

      :else (recur i (inc j) (if (and
                                   (re-find #".{18}#." (subs (nth picture i) j (+ j 20)))
                                   (re-find #"#.{4}##.{4}##.{4}###" (subs (nth picture (inc i)) j (+ j 20)))
                                   (re-find #".#.{2}#.{2}#.{2}#.{2}#.{2}#.{3}" (subs (nth picture (+ i 2)) j (+ j 20))))
                               (inc count)
                               count)))))

(defn solve-b
  []
  (let [tile-state (parse-input input)
        solved-puzzle (loop [row-i 0
                             col-i 1
                             ; Know that 2111 only has 2 neighbours => corner
                             ; Know that it has a match on top and right edge => rotate it to be 0 0 in row/col notation
                             image-state {[0 0] (reverse (get tile-state "2111"))}
                             remaining-tiles (dissoc tile-state "2111")]
                        (cond
                          (= row-i 12) image-state
                          (= col-i 12) (recur (inc row-i) 0 image-state remaining-tiles)
                          (= col-i 0) (let [previous-tile (get image-state [(dec row-i) 0])
                                            ; Bottom edge of previous
                                            edge-to-match (last previous-tile)
                                            [matching-id matching-tile] (get-matching-tile remaining-tiles edge-to-match)]
                                        ; Since we have matched on left edge but in this case it's a bottom edge we need to rotate the tile
                                        (recur row-i (inc col-i) (assoc image-state [row-i col-i] (rotate-tile matching-tile 4)) (dissoc remaining-tiles matching-id)))
                          :else (let [previous-tile (get image-state [row-i (dec col-i)])
                                      ; Right edge of previous
                                      edge-to-match (apply str (map last previous-tile))
                                      [matching-id matching-tile] (get-matching-tile remaining-tiles edge-to-match)]
                                  (recur row-i (inc col-i) (assoc image-state [row-i col-i] matching-tile) (dissoc remaining-tiles matching-id)))))
        full-picture (combine-full-picture solved-puzzle)
        number-of-monsters (loop [i 0
                                  max-monsters 0]
                             (if (= i 8)
                               max-monsters
                               (recur (inc i) (max max-monsters (count-sea-monsters (rotate-tile full-picture i))))))
        number-of-# (reduce (fn [sum row]
                              (+ sum (count (re-seq #"#" row))))
                            0
                            full-picture)]
    ; Each sea monster is 15 #
    (- number-of-# (* number-of-monsters 15))))

(comment
  (time
    (solve-b))
  ; 1559
  ; "Elapsed time: 1079.383947 msecs"
  )
