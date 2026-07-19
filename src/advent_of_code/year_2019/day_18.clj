(ns advent-of-code.year-2019.day-18
  (:require [ysera.test :refer [is= is is-not]]
            [advent-of-code.grid :refer [get-neighbours directions-with-only-diagonals]]))

(def input (slurp "src/advent_of_code/year_2019/inputs/day18.txt"))
(def test-input "########################\n#f.D.E.e.C.b.A.@.a.B.c.#\n######################.#\n#d.....................#\n########################")
(def test-input-2 "###############\n#d.ABC.#.....a#\n######@#@######\n###############\n######@#@######\n#b.....#.....c#\n###############")
(def test-input-3 "#############\n#g#f.D#..h#l#\n#F###e#E###.#\n#dCba@#@BcIJ#\n#############\n#nK.L@#@G...#\n#M###N#H###.#\n#o#m..#i#jk.#\n#############")

(defn parse-input
  [input]
  (let [lines (into [] (clojure.string/split-lines input))]
    (reduce-kv (fn [a i line]
                 (reduce-kv (fn [a j c]
                              (cond
                                (= c \#)
                                (update a :walls conj [i j])

                                (= c \@)
                                (assoc a :entrance [i j])

                                (re-matches #"[A-Z]" (str c))
                                (update a :doors assoc [i j] (str c))

                                (re-matches #"[a-z]" (str c))
                                (update a :keys assoc [i j] (str c))

                                :else
                                a))
                            a
                            (into [] line)))
               {:entrance nil
                :doors    {}
                :keys     {}
                :walls    #{}}
               lines)))

(defn remove-key
  [state k]
  (as-> (clojure.set/map-invert (:remaining-keys state)) $
        (dissoc $ k)
        (clojure.set/map-invert $)
        (assoc state :remaining-keys $)))

(defn update-position
  [state k]
  (as-> (clojure.set/map-invert (:remaining-keys state)) $
        (get $ k)
        (assoc state :position $)))

(defn finished?
  [states]
  (= #{} (ffirst (keys states))))

(defn get-answer
  [states]
  (->> (vals states)
       (map :steps)
       (sort)
       (first)))

(defn door-locked?
  [door remaining-keys]
  (contains? remaining-keys (clojure.string/lower-case door)))

(defn any-door-locked?
  [doors remaining-keys]
  (let [ks (into #{} (vals remaining-keys))]
    (some (fn [door]
            (door-locked? door ks))
          doors)))

;; This works because the maze only has one route to each key, so you don't have to consider the case where you might want to take a longer route to avoid a locked door.
(defn get-distances-to-keys
  [the-map position ks]
  (let [{walls :walls
         doors :doors} the-map]
    (loop [positions #{{:position position :doors-passed #{}}}
           visited #{position}
           steps 0
           distances {}]
      (if (empty? positions)
        distances
        (let [steps (inc steps)
              positions (reduce (fn [a {position :position doors-passed :doors-passed}]
                                  (->> (get-neighbours position)
                                       (remove (fn [neighbour]
                                                 (or (contains? walls neighbour)
                                                     (contains? visited neighbour))))
                                       (map (fn [neighbour]
                                              (if (contains? doors neighbour)
                                                {:position neighbour :doors-passed (conj doors-passed (get doors neighbour))}
                                                {:position neighbour :doors-passed doors-passed})))
                                       (reduce conj a)))
                                #{}
                                positions)
              positions-with-keys (filter (fn [{position :position}]
                                            (contains? ks (get-in the-map [:keys position])))
                                          positions)]
          (recur positions
                 (reduce conj visited (map :position positions))
                 steps
                 (reduce (fn [distances {position :position doors-passed :doors-passed}]
                           (assoc distances (get-in the-map [:keys position]) {:distance     steps
                                                                               :doors-passed doors-passed}))
                         distances
                         positions-with-keys)))))))

(defn get-key-distances
  [the-map]
  (let [key-positions (clojure.set/map-invert (:keys the-map))
        ks (into #{} (keys key-positions))]
    (reduce (fn [a k]
              (assoc a k (get-distances-to-keys the-map (get key-positions k) (disj ks k))))
            {:entrance (get-distances-to-keys the-map (:entrance the-map) ks)}
            ks)))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 86))}
  [input]
  (let [the-map (parse-input input)
        key-distances (get-key-distances the-map)
        ks (into #{} (vals (:keys the-map)))]
    (loop [states {[ks (:entrance the-map)] {:position       (:entrance the-map)
                                             :remaining-keys (:keys the-map)
                                             :steps          0}}]
      (if (finished? states)
        (get-answer states)
        (recur (reduce (fn [states state]
                         (reduce-kv (fn [states k {distance :distance doors-passed :doors-passed}]
                                      ;; If k already visited, or any door locked on the way -> skip k
                                      (if (or (not (contains? (into #{} (vals (:remaining-keys state))) k))
                                              (any-door-locked? doors-passed (:remaining-keys state)))
                                        states
                                        (let [s (-> state
                                                    (update-position k)
                                                    (remove-key k)
                                                    (update :steps + distance))
                                              remaining-keys (into #{} (vals (:remaining-keys s)))
                                              position (:position s)]
                                          (if (and (contains? states [remaining-keys position])
                                                   (< (get-in states [[remaining-keys position] :steps])
                                                      (:steps s)))
                                            states
                                            (assoc states [remaining-keys position] s)))))
                                    states
                                    (get key-distances (get-in the-map [:keys (:position state)] :entrance))))
                       {}
                       (vals states)))))))

(defn parse-input-2
  [input]
  (let [lines (into [] (clojure.string/split-lines input))]
    (reduce-kv (fn [a i line]
                 (reduce-kv (fn [a j c]
                              (cond
                                (= c \#)
                                (update a :walls conj [i j])

                                (= c \@)
                                (update a :entrances conj [i j])

                                (re-matches #"[A-Z]" (str c))
                                (update a :doors assoc [i j] (str c))

                                (re-matches #"[a-z]" (str c))
                                (update a :keys assoc [i j] (str c))

                                :else
                                a))
                            a
                            (into [] line)))
               {:entrances []
                :doors     {}
                :keys      {}
                :walls     #{}}
               lines)))

(defn get-key-distances-2
  [the-map]
  (let [key-positions (clojure.set/map-invert (:keys the-map))
        ks (into #{} (keys key-positions))]
    (reduce (fn [a k]
              (assoc a k (get-distances-to-keys the-map (get key-positions k) (disj ks k))))
            (zipmap (:entrances the-map) (map (fn [entrance]
                                                (get-distances-to-keys the-map entrance ks))
                                              (:entrances the-map)))
            ks)))

(defn update-positions
  [state position-index k]
  (as-> (clojure.set/map-invert (:remaining-keys state)) $
        (get $ k)
        (assoc-in state [:positions position-index] $)))

(defn solve-multi-vault
  {:test (fn []
           (is= (solve-multi-vault (parse-input-2 test-input-2)) 24)
           (is= (solve-multi-vault (parse-input-2 test-input-3)) 72))}
  [the-map]
  (let [key-distances (get-key-distances-2 the-map)
        ks (into #{} (vals (:keys the-map)))]
    (loop [states {[ks (:entrances the-map)] {:positions      (:entrances the-map)
                                              :remaining-keys (:keys the-map)
                                              :steps          0}}]
      (if (finished? states)
        (get-answer states)
        (recur (reduce (fn [states state]
                         (reduce (fn [states position-index]
                                   (let [position (get-in state [:positions position-index])]
                                     (reduce-kv (fn [states k {distance :distance doors-passed :doors-passed}]
                                                  ;; If k already visited, or any door locked on the way -> skip k
                                                  (if (or (not (contains? (into #{} (vals (:remaining-keys state))) k))
                                                          (any-door-locked? doors-passed (:remaining-keys state)))
                                                    states
                                                    (let [s (-> state
                                                                (update-positions position-index k)
                                                                (remove-key k)
                                                                (update :steps + distance))
                                                          remaining-keys (into #{} (vals (:remaining-keys s)))
                                                          positions (:positions s)]
                                                      (if (and (contains? states [remaining-keys positions])
                                                               (< (get-in states [[remaining-keys positions] :steps])
                                                                  (:steps s)))
                                                        states
                                                        (assoc states [remaining-keys positions] s)))))
                                                states
                                                (get key-distances (get-in the-map [:keys position] position)))))
                                 states
                                 (range 4)))
                       {}
                       (vals states)))))))

(defn convert-to-p2-map
  [the-map]
  (-> the-map
      (dissoc :entrance)
      (update :walls (fn [walls]
                       (reduce conj walls (conj (get-neighbours (:entrance the-map))
                                                (:entrance the-map)))))
      (assoc :entrances (into [] (get-neighbours (:entrance the-map) directions-with-only-diagonals)))))

(defn part-2
  [input]
  (-> (parse-input input)
      (convert-to-p2-map)
      (solve-multi-vault)))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 3567.125125 msecs"
  ;=> 3216

  (time (part-2 input))
  ;; "Elapsed time: 8901.482709 msecs"
  ;=> 1538
  )
