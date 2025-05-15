(ns advent-of-code.year-2015.day-22
  (:require [ysera.test :refer [is= is is-not]]
            [clojure.data.priority-map :refer [priority-map]]))

(def input (slurp "src/advent_of_code/year_2015/inputs/day22.txt"))

(defn get-player-armour
  [state]
  (if (> (:turns-shield state) 0)
    7
    0))

(defn damage-dealt
  {:test (fn []
           (is= (damage-dealt 8 3) 5)
           (is= (damage-dealt 8 300) 1))}
  [damage armour]
  (max 1 (- damage armour)))

(defn boss-attack
  [state boss-damage]
  (update state :health - (damage-dealt boss-damage (get-player-armour state))))

(defn won?
  [state]
  (>= 0 (:boss-health state)))

(defn lost?
  [state]
  (>= 0 (:health state)))

(defn cast-magic-missile
  [state]
  (when (>= (:mana state) 53)
    (-> state
        (update :mana - 53)
        (update :boss-health - 4))))

(defn cast-drain
  [state]
  (when (>= (:mana state) 73)
    (-> state
        (update :mana - 73)
        (update :boss-health - 2)
        (update :health + 2))))

(defn cast-shield
  [state]
  (when (and (>= (:mana state) 113)
             (zero? (:turns-shield state)))
    (-> state
        (update :mana - 113)
        (assoc :turns-shield 6))))

(defn cast-poison
  [state]
  (when (and (>= (:mana state) 173)
             (zero? (:turns-poison state)))
    (-> state
        (update :mana - 173)
        (assoc :turns-poison 6))))

(defn cast-recharge
  [state]
  (when (and (>= (:mana state) 229)
             (zero? (:turns-recharge state)))
    (-> state
        (update :mana - 229)
        (assoc :turns-recharge 5))))

(def spells-with-mana-cost [[cast-magic-missile 53]
                            [cast-drain 73]
                            [cast-shield 113]
                            [cast-poison 173]
                            [cast-recharge 229]])

(defn apply-shield-effect
  [state]
  (if (zero? (:turns-shield state))
    state
    (update state :turns-shield dec)))

(defn apply-poison-effect
  [state]
  (if (zero? (:turns-poison state))
    state
    (-> state
        (update :boss-health - 3)
        (update :turns-poison dec))))

(defn apply-recharge-effect
  [state]
  (if (zero? (:turns-recharge state))
    state
    (-> state
        (update :mana + 101)
        (update :turns-recharge dec))))

(def turn-effects [apply-shield-effect apply-poison-effect apply-recharge-effect])

(defn apply-turn-effects
  [state]
  (reduce (fn [state effect-fn]
            (effect-fn state))
          state
          turn-effects))

(defn hard-mode-effect
  [state]
  (update state :health dec))

(defn play-game
  {:test (fn []
           (is= (play-game 10 250 13 8 false) 226)
           (is= (play-game 10 250 14 8 false) 641))}
  [player-health player-mana boss-health boss-damage hard-mode]
  (loop [states (priority-map {:health player-health :mana player-mana :turns-shield 0 :turns-poison 0 :turns-recharge 0 :boss-health boss-health} 0)]
    (let [[state mana-spent] (peek states)
          states (pop states)]
      (if (won? state)
        mana-spent
        (let [state (if hard-mode (hard-mode-effect state) state)]
          (if (lost? state)
            (recur states)
            (let [state (apply-turn-effects state)]
              (if (won? state)
                mana-spent
                (let [states-and-mana-spent-after-spell-cast (reduce (fn [a [spell mana-cost]]
                                                                       (if-let [state (spell state)]
                                                                         (conj a [state (+ mana-spent mana-cost)])
                                                                         a))
                                                                     []
                                                                     spells-with-mana-cost)
                      states-and-mana-spent-after-boss-turn (keep (fn [[state mana-spent]]
                                                                    (let [state (apply-turn-effects state)]
                                                                      (if (won? state)
                                                                        [state mana-spent]
                                                                        (let [state (boss-attack state boss-damage)]
                                                                          (when-not (lost? state)
                                                                            [state mana-spent])))))
                                                                  states-and-mana-spent-after-spell-cast)
                      states (reduce (fn [a [state mana-spent]]
                                       (assoc a state mana-spent))
                                     states
                                     states-and-mana-spent-after-boss-turn)]
                  (recur states))))))))))

(defn part-1
  [input]
  (let [[boss-health boss-damage] (map read-string (re-seq #"\d+" input))]
    (play-game 50 500 boss-health boss-damage false)))

(defn part-2
  [input]
  (let [[boss-health boss-damage] (map read-string (re-seq #"\d+" input))]
    (play-game 50 500 boss-health boss-damage true)))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 33.016875 msecs"
  ;=> 953

  (time (part-2 input))
  ;; "Elapsed time: 39.710458 msecs"
  ;=> 1289
  )