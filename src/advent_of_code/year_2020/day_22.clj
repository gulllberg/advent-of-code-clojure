(ns advent-of-code.year-2020.day-22
  (:require [ysera.test :refer [is= is is-not]]))

(def player-1-input (slurp "src/advent_of_code/year_2020/inputs/day22player1.txt"))
(def player-2-input (slurp "src/advent_of_code/year_2020/inputs/day22player2.txt"))

(defn play-game
  [deck1 deck2]
  (loop [deck1 deck1
         deck2 deck2]
    (cond
      (empty? deck1)
      [:p2 deck2]

      (empty? deck2)
      [:p1 deck1]

      :else (let [c1 (first deck1)
                  c2 (first deck2)]
              (if (> c1 c2)
                (recur (concat (rest deck1) [c1 c2]) (rest deck2))
                (recur (rest deck1) (concat (rest deck2) [c2 c1])))))))

(defn score-deck
  {:test (fn []
           (is= (score-deck [7 5 6 2 4 1 10 8 9 3]) 291))}
  [deck]
  (reduce (fn [score i]
            (+ score (* (inc i) (nth (reverse deck) i))))
          0
          (range (count deck))))

(defn solve-a
  []
  (-> (play-game (map read-string (clojure.string/split-lines player-1-input)) (map read-string (clojure.string/split-lines player-2-input)))
      (second)
      (score-deck)))

(comment
  (solve-a)
  ; 31809
  )

(defn play-recursive-game
  {:test (fn []
           (is= (play-recursive-game [9 2 6 3 1] [5 8 4 7 10]) [:p2 [7 5 6 2 4 1 10 8 9 3]]))}
  [deck1 deck2]
  (loop [deck1 deck1
         deck2 deck2
         previous-states #{}]
    (let [state (str (clojure.string/join "," deck1) "+" (clojure.string/join "," deck2))]
      (cond
        (contains? previous-states state)
        [:p1 deck1]

        (empty? deck1)
        [:p2 deck2]

        (empty? deck2)
        [:p1 deck1]

        :else (let [c1 (first deck1)
                    c2 (first deck2)]
                (if (and (>= (count (rest deck1)) c1) (>= (count (rest deck2)) c2))
                  (let [winner (first (play-recursive-game (take c1 (rest deck1)) (take c2 (rest deck2))))]
                    (if (= winner :p1)
                      (recur (concat (rest deck1) [c1 c2]) (rest deck2) (conj previous-states state))
                      (recur (rest deck1) (concat (rest deck2) [c2 c1]) (conj previous-states state))))
                  (if (> c1 c2)
                    (recur (concat (rest deck1) [c1 c2]) (rest deck2) (conj previous-states state))
                    (recur (rest deck1) (concat (rest deck2) [c2 c1]) (conj previous-states state)))))))))

(defn solve-b
  []
  (-> (play-recursive-game (map read-string (clojure.string/split-lines player-1-input)) (map read-string (clojure.string/split-lines player-2-input)))
      (second)
      (score-deck)))

(comment
  (time
    (solve-b))
  ; 32835
  ; "Elapsed time: 4159.820439 msecs"
  )
