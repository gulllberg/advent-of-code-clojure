(ns advent-of-code.year-2018.day-24
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2018/inputs/day24.txt"))
(def test-input "Immune System:\n17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2\n989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3\n\nInfection:\n801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1\n4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4")

(defn parse-immunities-or-weaknesses
  [immunities-or-weaknesses]
  (->> (clojure.string/split immunities-or-weaknesses #", ")
       (map keyword)
       (into #{})))

(defn parse-immunities-and-weaknesses
  [immunities-and-weaknesses]
  (let [immunities-match (re-find #"immune to ([^);]+)" immunities-and-weaknesses)
        weaknesses-match (re-find #"weak to ([^);]+)" immunities-and-weaknesses)]
    (merge {}
           (when immunities-match {:immune (parse-immunities-or-weaknesses (second immunities-match))})
           (when weaknesses-match {:weakness (parse-immunities-or-weaknesses (second weaknesses-match))}))))

(defn parse-group
  [group-match group-id]
  (let [[_ units hit-points immunities-and-weaknesses attack attack-type initiative] group-match]
    (merge {:id         group-id
            :units      (read-string units)
            :hit-points (read-string hit-points)
            :attack     (read-string attack)
            :type       (keyword attack-type)
            :initiative (read-string initiative)}
           (when immunities-and-weaknesses (parse-immunities-and-weaknesses immunities-and-weaknesses)))))

(defn parse-groups
  [input army-id]
  (let [groups (re-seq #"(\d+) units each with (\d+) hit points(?: (\([^)]+\)))? with an attack that does (\d+) (\w+) damage at initiative (\d+)" input)]
    (reduce (fn [a i]
              (let [group-id (keyword (str "group" i))]
                (assoc a group-id (parse-group (nth groups i) [army-id group-id]))))
            {}
            (range (count groups)))))

(defn parse-input
  [input]
  (let [[immune-part infection-part] (clojure.string/split input #"\n\n")]
    {:immune    (parse-groups immune-part :immune)
     :infection (parse-groups infection-part :infection)}))

;; All the game logic came from Tomas
(defn calculate-damage
  {:test (fn []
           (let [test-state (parse-input test-input)]
             (is= (calculate-damage test-state [:infection :group0] [:immune :group0]) 185832)
             (is= (calculate-damage test-state [:infection :group0] [:immune :group1]) 185832)
             (is= (calculate-damage test-state [:infection :group1] [:immune :group1]) 107640)
             (is= (calculate-damage test-state [:immune :group0] [:infection :group0]) 76619)
             (is= (calculate-damage test-state [:immune :group0] [:infection :group1]) 153238)
             (is= (calculate-damage test-state [:immune :group1] [:infection :group0]) 24725)
             (is= (calculate-damage test-state [:immune :group1] [:infection :group1]) 24725)))}
  [state attacker-id defender-id]
  (let [attacker (get-in state attacker-id)
        defender (get-in state defender-id)]
    (if (contains? (:immune defender) (:type attacker))
      0
      (* (:units attacker)
         (:attack attacker)
         (if (contains? (:weakness defender) (:type attacker)) 2 1)))))

(defn get-killed-units
  {:test (fn []
           (let [test-state (parse-input test-input)]
             (is= (get-killed-units test-state [:infection :group1] [:immune :group1]) 84)
             (is= (get-killed-units test-state [:immune :group0] [:infection :group1]) 51)
             (is= (get-killed-units test-state [:infection :group0] [:immune :group0]) 17)))}
  [state attacker-id defender-id]
  (let [defender (get-in state defender-id)
        damage (calculate-damage state attacker-id defender-id)
        killed-units (min (quot damage (:hit-points defender))
                          (:units defender))]
    ;(println "A" attacker-id "D" defender-id "damage" damage "ku" killed-units)
    killed-units))

(defn get-group-ids
  [state]
  (->> (concat (vals (:immune state)) (vals (:infection state)))
       (map :id)
       (set)))

(defn get-group
  [state id]
  (->> (concat (vals (:immune state)) (vals (:infection state)))
       (some (fn [g] (when (= (:id g) id) g)))))

(defn get-effective-power
  [state id]
  (let [group (get-group state id)]
    (* (:units group) (:attack group))))

(defn get-initiative
  [state id]
  (let [group (get-group state id)]
    (:initiative group)))

(defn get-target-selection-order
  {:test (fn []
           (is= (get-target-selection-order (parse-input test-input))
                [[:infection :group0] [:immune :group0] [:infection :group1] [:immune :group1]]))}
  [state]
  (->> (concat (vals (:immune state)) (vals (:infection state)))
       (map (fn [g] (assoc g :effective-power (* (:units g) (:attack g)))))
       (sort-by (juxt :effective-power :initiative) #(compare %2 %1))
       (map :id)))

(defn get-initiative-rank
  [state]
  (->> (concat (vals (:immune state)) (vals (:infection state)))
       (sort-by :initiative)
       (reverse)
       (map :id)))

(defn same-team?
  [group1 group2]
  (= (first group1) (first group2)))

(defn target-selection
  {:test (fn []
           (is= (target-selection (parse-input test-input))
                {[:immune :group1]    [:infection :group0]
                 [:infection :group1] [:immune :group1]
                 [:immune :group0]    [:infection :group1]
                 [:infection :group0] [:immune :group0]}))}
  [state]
  (let [effective-power-rank (get-target-selection-order state)
        group-ids (get-group-ids state)]
    (->> effective-power-rank
         (reduce (fn [a g]
                   (let [defender (->> (:defender-ids a)
                                       (remove (fn [og] (same-team? g og)))
                                       (map (fn [d] {:id              d
                                                     :damage          (calculate-damage state g d)
                                                     :effective-power (get-effective-power state d)
                                                     :initiative      (get-initiative state d)}))
                                       (remove (fn [d] (zero? (:damage d))))
                                       ;; BUGG HÄR! Saknar :initiative
                                       (sort-by (juxt :damage :effective-power :initiative) #(compare %2 %1))
                                       (first)
                                       (:id))]
                     (if defender
                       (-> a
                           (update :result assoc g defender)
                           (update :defender-ids disj defender))
                       a)))
                 {:result       {}
                  :defender-ids group-ids})
         (:result))))

(defn attack
  [state attacker-id defender-id]
  (let [attacker (get-group state attacker-id)
        defender (get-group state defender-id)]
    (if-not (and attacker defender)
      state
      (let [killed-units (get-killed-units state attacker-id defender-id)
            existing-units (:units (get-in state defender-id))]
        (if (> existing-units killed-units)
          (update-in state defender-id
                     (fn [defender]
                       (update defender :units - killed-units)))
          (update state (first defender-id)
                  (fn [team] (dissoc team (second defender-id)))))))))

(defn attack-phase
  [state planned-attacks]
  (let [initiative-rank (get-initiative-rank state)]
    (reduce (fn [state attacker-id]
              (attack state attacker-id (get planned-attacks attacker-id)))
            state
            initiative-rank)))

(defn play-a-round
  {:test (fn []
           (is= (play-a-round (parse-input test-input))
                {:immune    {:group1 {:id         [:immune :group1]
                                      :units      905
                                      :hit-points 1274
                                      :immune     #{:fire}
                                      :weakness   #{:bludgeoning :slashing}
                                      :attack     25
                                      :type       :slashing
                                      :initiative 3}}
                 :infection {:group0 {:id         [:infection :group0]
                                      :units      797
                                      :hit-points 4706
                                      :weakness   #{:radiation}
                                      :attack     116
                                      :type       :bludgeoning
                                      :initiative 1}
                             :group1 {:id         [:infection :group1]
                                      :units      4434
                                      :hit-points 2961
                                      :immune     #{:radiation}
                                      :weakness   #{:fire :cold}
                                      :attack     12
                                      :type       :slashing
                                      :initiative 4}}}))}
  [state]
  (let [planned-attacks (target-selection state)]
    ;(println "planned attacks:" planned-attacks)
    (attack-phase state planned-attacks)))

(defn end?
  [state]
  (or (= (get state :immune) {})
      (= (get state :infection) {})))

;(defn get-units
;  [state]
;  (->> (get-group-ids state)
;       (reduce (fn [a v]
;                 (assoc a v (:units (get-group state v))))
;               {})))

(defn count-units
  [state]
  (->> (get-group-ids state)
       (map (fn [id] (get-group state id)))
       (map :units)
       (apply +)))

(defn play
  {:test (fn []
           (is= (play (parse-input test-input))
                {:immune    {},
                 :infection {:group0 {:id         [:infection :group0],
                                      :units      782,
                                      :hit-points 4706,
                                      :weakness   #{:radiation},
                                      :attack     116,
                                      :type       :bludgeoning,
                                      :initiative 1},
                             :group1 {:id         [:infection :group1],
                                      :units      4434,
                                      :hit-points 2961,
                                      :immune     #{:radiation},
                                      :weakness   #{:fire :cold},
                                      :attack     12,
                                      :type       :slashing,
                                      :initiative 4}}}))}
  [state]
  (let [new-state (play-a-round state)]
    ;(println (get-units new-state))
    ;(println (count-units new-state))
    (cond
      (= state new-state) state
      (end? new-state) new-state
      :else (recur new-state))))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input)
                5216))}
  [input]
  (-> (parse-input input)
      (play)
      (count-units)))

(defn boost-immune-system
  [state boost]
  (update state :immune (fn [immune-system]
                          (reduce-kv (fn [a k v]
                                       (assoc a k (update v :attack + boost)))
                                     {}
                                     immune-system))))

(defn immune-won?
  [state]
  (= (get state :infection) {}))

(defn binary-search
  [state]
  (loop [lower 0
         higher 2048]
    (let [curr (long (Math/floor (/ (+ lower higher) 2)))
          _ (println lower higher curr)
          end-state (play (boost-immune-system state curr))]
      (cond
        (and (immune-won? end-state)
             (= curr lower))
        (count-units end-state)

        (and (not (immune-won? end-state))
             (= (inc curr) higher))
        (count-units (play (boost-immune-system state higher)))

        (immune-won? end-state)
        (recur lower curr)

        :else
        (recur curr higher)))))

(defn loop-search
  [state]
  (loop [boost 1]
    ;(println boost)
    (let [end-state (play (boost-immune-system state boost))]
      (if (immune-won? end-state)
        (count-units end-state)
        (recur (inc boost))))))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input)
                51))}
  [input]
  (-> (parse-input input)
      (binary-search)
      ;(loop-search)
      ))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 75.292542 msecs"
  ;=> 16530

  (time (part-2 input))
  ;; "Elapsed time: 1099.601083 msecs"
  ;=> 3313

  ; this one ends in deadlock
  ;(play (boost-immune-system (parse-input input) 40))
  )
