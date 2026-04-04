(ns advent-of-code.year_2018.day_13
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2018/inputs/day13.txt"))
(def test-input "/->-\\        \n|   |  /----\\\n| /-+--+-\\  |\n| | |  | v  |\n\\-+-/  \\-+--/\n  \\------/   ")
(def test-input-2 "/>-<\\  \n|   |  \n| /<+-\\\n| | | v\n\\>+</ |\n  |   ^\n  \\<->/")

(def cart->direction {\v [1 0]
                      \^ [-1 0]
                      \< [0 -1]
                      \> [0 1]})

(def cart->track {\v \|
                  \^ \|
                  \< \-
                  \> \-})

(defn parse-input
  [input]
  (let [lines (into [] (clojure.string/split-lines input))]
    (reduce-kv (fn [a i line]
                 (reduce-kv (fn [a j c]
                              (cond
                                (contains? #{\/ \\ \- \| \+} c)
                                (update a :grid assoc [i j] c)

                                (contains? #{\v \^ \< \>} c)
                                (-> (update a :grid assoc [i j] (cart->track c))
                                    (update :carts conj {:position [i j] :direction (cart->direction c) :next-turn :left}))

                                :else a))
                            a
                            (into [] line)))
               {:grid {} :carts #{}}
               lines)))

(defn get-new-position
  [position new-direction]
  (mapv + position new-direction))

(defn turn-right
  [direction]
  [(second direction) (* -1 (first direction))])

(defn turn-left
  [direction]
  [(* -1 (second direction)) (first direction)])

(defn get-new-direction
  [track direction next-turn]
  (cond
    (contains? #{\| \-} track) direction
    (= track \/) (get {[-1 0] [0 1]
                       [1 0]  [0 -1]
                       [0 -1] [1 0]
                       [0 1]  [-1 0]} direction)
    (= track \\) (get {[-1 0] [0 -1]
                       [1 0]  [0 1]
                       [0 -1] [-1 0]
                       [0 1]  [1 0]} direction)
    ;; + intersection
    (= next-turn :right) (turn-right direction)
    (= next-turn :left) (turn-left direction)
    ;; :straight - no direction change
    :else direction))

(defn get-new-next-turn
  [track next-turn]
  (if (not= track \+)
    next-turn
    (next-turn {:left     :straight
                :straight :right
                :right    :left})))

(defn move-cart
  [grid {position :position direction :direction next-turn :next-turn}]
  (let [track (get grid position)
        new-direction (get-new-direction track direction next-turn)
        new-next-turn (get-new-next-turn track next-turn)
        new-position (get-new-position position new-direction)]
    {:position new-position :direction new-direction :next-turn new-next-turn}))

(defn sort-carts
  [carts]
  (sort-by (juxt (fn [{position :position}] (first position)) (fn [{position :position}] (second position))) carts))

(defn get-collision-position
  [carts]
  (->> (map :position carts)
       (frequencies)
       (reduce-kv (fn [_ k v]
                    (when (> v 1)
                      (reduced k)))
                  nil)))

(defn move-until-collision
  [grid carts]
  (loop [carts carts
         carts-move-queue []]
    (if (empty? carts-move-queue)
      (recur carts (sort-carts carts))
      (let [cart (first carts-move-queue)
            moved-cart (move-cart grid cart)
            carts (-> (disj carts cart)
                      (conj moved-cart))
            collision-position (get-collision-position carts)]
        (if collision-position
          collision-position
          (recur carts (rest carts-move-queue)))))))

(defn get-answer-from-position
  [position]
  (str (second position) "," (first position)))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) "7,3"))}
  [input]
  (let [{grid :grid carts :carts} (parse-input input)
        collision-position (move-until-collision grid carts)]
    (get-answer-from-position collision-position)))

(defn remove-carts-in-position
  [carts position]
  (->> carts
       (remove (fn [{p :position}]
                 (= p position)))
       (into #{})))

(defn remove-carts-in-position-from-queue
  [carts-queue position]
  (remove (fn [{p :position}]
            (= p position))
          carts-queue))

(defn move-until-one-cart-left
  [grid carts]
  (loop [carts carts
         carts-move-queue []]
    (if (empty? carts-move-queue)
      (if (= 1 (count carts))
        (:position (first carts))
        (recur carts (sort-carts carts)))
      (let [cart (first carts-move-queue)
            moved-cart (move-cart grid cart)
            carts (-> (disj carts cart)
                      (conj moved-cart))
            collision-position (get-collision-position carts)]
        (if collision-position
          (recur (remove-carts-in-position carts collision-position)
                 (remove-carts-in-position-from-queue (rest carts-move-queue) collision-position))
          (recur carts (rest carts-move-queue)))))))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input-2) "6,4"))}
  [input]
  (let [{grid :grid carts :carts} (parse-input input)
        remaining-position (move-until-one-cart-left grid carts)]
    (get-answer-from-position remaining-position)))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 41.123291 msecs"
  ;=> "91,69"

  (time (part-2 input))
  ;; "Elapsed time: 168.106083 msecs"
  ;=> "44,87"
  )

