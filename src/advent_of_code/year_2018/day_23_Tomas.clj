(ns advent-of-code.year_2018.day_23_Tomas
  (:require [ysera.test :refer [is= is is-not]]
            [clojure.string :refer [split-lines]]
            [clojure.edn :as edn]))

(def input (->> (slurp "src/advent_of_code/year_2018/inputs/day23.txt")
                (split-lines)))

(def test-input ["pos=<0,0,0>, r=4"
                 "pos=<1,0,0>, r=1"
                 "pos=<4,0,0>, r=3"
                 "pos=<0,2,0>, r=1"
                 "pos=<0,5,0>, r=3"
                 "pos=<0,0,3>, r=1"
                 "pos=<1,1,1>, r=1"
                 "pos=<1,1,2>, r=1"
                 "pos=<1,3,1>, r=1"])

(defn parse-nanobots
  {:test (fn []
           (is= (parse-nanobots test-input)
                [{:position [0 0 0], :radius 4}
                 {:position [1 0 0], :radius 1}
                 {:position [4 0 0], :radius 3}
                 {:position [0 2 0], :radius 1}
                 {:position [0 5 0], :radius 3}
                 {:position [0 0 3], :radius 1}
                 {:position [1 1 1], :radius 1}
                 {:position [1 1 2], :radius 1}
                 {:position [1 3 1], :radius 1}]))}
  [input]
  (->> input
       (map (fn [row]
              (let [[x y z r] (->> (re-seq #"-?\d+" row)
                                   (map edn/read-string))]
                {:position [x y z] :radius r})))))

(def test-nanobots (parse-nanobots test-input))
(def nanobots (parse-nanobots input))

(defn get-strongest-nanobot
  {:test (fn []
           (is= (get-strongest-nanobot test-nanobots)
                {:position [0 0 0], :radius 4}))}
  [nanobots]
  (->> nanobots
       (reduce (fn [a n] (if (> (:radius n) (:radius a)) n a))
               (first nanobots))))

(def strongest-test-nanobot (get-strongest-nanobot test-nanobots))
(def strongest-nanobot (get-strongest-nanobot nanobots))

(defn manhattan-distance
  {:test (fn []
           (is= (manhattan-distance [3 1 6] [2 3 -1]) 10))}
  [p1 p2]
  (->> (map - p1 p2)
       (map abs)
       (reduce +)))

(defn in-range?
  {:test (fn []
           (is (in-range? [0 0 0] [1 0 0] 4))
           (is-not (in-range? [0 0 0] [0 2 0] 1)))}
  [position other-position signal-radius]
  (<= (manhattan-distance position other-position) signal-radius))

(defn nanobots-in-range
  {:test (fn []
           (is= (count (nanobots-in-range test-nanobots strongest-test-nanobot))
                7))}
  [nanobots nanobot]
  (->> nanobots
       (filter (fn [n] (in-range? (:position nanobot) (:position n) (:radius nanobot))))))

(comment
  (time (count (nanobots-in-range nanobots strongest-nanobot)))
  ;; "Elapsed time: 0.832292 msecs"
  ;=> 602
  )

; part 2

(def test-input-2 ["pos=<10,12,12>, r=2"
                   "pos=<12,14,12>, r=2"
                   "pos=<16,12,12>, r=4"
                   "pos=<14,14,14>, r=6"
                   "pos=<50,50,50>, r=200"
                   "pos=<10,10,10>, r=5"])

(def test-nanobots-2 (parse-nanobots test-input-2))

(defn find-bounding-rectangle
  {:test (fn []
           (is= (find-bounding-rectangle test-nanobots-2)
                [[10 10 10] [50 50 50]])
           (is= (find-bounding-rectangle test-nanobots)
                [[0 0 0] [4 5 3]]))}
  [nanobots]
  (reduce (fn [a {position :position}]
            (-> a
                (update 0 (fn [min-values] (mapv min min-values position)))
                (update 1 (fn [max-values] (mapv max max-values position)))))
          [(:position (first nanobots)) (:position (first nanobots))]
          nanobots))

(defn get-range-step
  {:test (fn []
           (is= (get-range-step 10 50 10) 4)
           (is= (get-range-step 10 14 10) 1))}
  [start stop max-steps]
  (let [distance (- stop start)
        step (quot distance max-steps)]
    (if (zero? step) 1 step)))

(get-range-step 0 93 10)
(range 0 94 9)

(defn third [[_ _ z]] z)

(defn find-better-rectangle
  ;{:test (fn []
  ;         (is= (find-better-rectangle test-nanobots-2 [[10 10 10] [50 50 50]] 10)
  ;              [[6 6 6] [14 14 14]])
  ;         (is= (find-better-rectangle test-nanobots-2 [[8 8 8] [12 12 12]] 10)
  ;              {:done [12 12 12]}))}
  [nanobots rectangle max-steps]
  (let [x-range (mapv first rectangle)
        y-range (mapv second rectangle)
        z-range (mapv third rectangle)
        x-range-step (apply get-range-step (conj x-range max-steps))
        y-range-step (apply get-range-step (conj y-range max-steps))
        z-range-step (apply get-range-step (conj z-range max-steps))
        best-positions (->> (for [x (range (first x-range) (+ (second x-range) x-range-step) x-range-step)
                                  y (range (first y-range) (+ (second y-range) y-range-step) y-range-step)
                                  z (range (first z-range) (+ (second z-range) z-range-step) z-range-step)]
                              [x y z])
                            (reduce (fn [a position]
                                      (let [number-in-range (->> nanobots
                                                                 (filter (fn [n] (in-range? (:position n) position (:radius n))))
                                                                 (count))]
                                        (when (> number-in-range 875) (println number-in-range position))
                                        (if (< number-in-range (* 0.8 (:best-number-in-range a)))
                                          a
                                          {:best-number-in-range (max number-in-range (:best-number-in-range a))
                                           :positions            (conj (:positions a) {:position position :number-in-range number-in-range})})))
                                    {:best-number-in-range 0
                                     :positions            (sorted-set-by (fn [a b] (compare (:number-in-range b) (:number-in-range a))))})
                            (:positions))
        bounding-rectangle (find-bounding-rectangle (take 5 best-positions))
        steps [x-range-step y-range-step z-range-step]
        modified-bounding-rectangle (-> bounding-rectangle
                                        (update 0 (fn [min-corner] (mapv - min-corner steps)))
                                        (update 1 (fn [max-corner] (mapv + max-corner steps))))]
    modified-bounding-rectangle
    ;(if (= x-range-step y-range-step z-range-step 1)
    ;  {:done best-position}
    ;  [[(- (first best-position) (* 3 x-range-step))
    ;    (- (second best-position) (* 3 y-range-step))
    ;    (- (third best-position) (* 3 z-range-step))]
    ;   [(+ (first best-position) (* 3 x-range-step))
    ;    (+ (second best-position) (* 3 y-range-step))
    ;    (+ (third best-position) (* 3 z-range-step))]])
    ))

(defn find-coordinates-in-range-of-the-largets-number-of-nanobots
  ;{:test (fn []
  ;         (is= (find-coordinates-in-range-of-the-largets-number-of-nanobots test-nanobots-2)
  ;              36))}
  ([nanobots]
   (find-coordinates-in-range-of-the-largets-number-of-nanobots
     nanobots
     (find-bounding-rectangle nanobots) 30))
  ([nanobots initial-rectangle max-steps]
   (loop [rectangle initial-rectangle]
     (println rectangle)
     (if (:done rectangle)
       (do (println (:done rectangle))
           (manhattan-distance [0 0 0] (:done rectangle)))
       (recur (find-better-rectangle nanobots rectangle max-steps))))))

(defn find-number-of-nanobots-in-range
  [nanobots position]
  (->> nanobots
       (filter (fn [n] (in-range? (:position n) position (:radius n))))
       (count)))

(comment
  (time (find-coordinates-in-range-of-the-largets-number-of-nanobots nanobots))
  ; 100592453 [14315582 59365374 26911497]
  ; 20 -> 100607191 [14306083 59372743 26928365]
  ; 100038641 [14950629 58303297 26784715]

  (def initial-rectangle (find-bounding-rectangle nanobots))
  (find-better-rectangle nanobots initial-rectangle 25)
  (find-better-rectangle nanobots
                         [[22064759 8038441 25926896] [66584639 66505541 67530260]]
                         25)
  (find-better-rectangle nanobots
                         [[25626349 38441333 37575834] [34530324 45457385 44232370]]
                         25)
  (find-better-rectangle nanobots
                         [[25982508 43492889 39173400] [30256416 45176741 41303488]]
                         25)
  (find-better-rectangle nanobots
                         [[25982508 44166429 39599415] [26666332 44974677 40451445]]
                         25)
  (find-better-rectangle nanobots
                         [[26037212 44166429 39633496] [26474844 45006983 40417359]]
                         25)
  (find-better-rectangle nanobots
                         [[26037212 44368161 39602142] [26352302 45040601 40385992]]
                         25)
  (find-better-rectangle nanobots
                         [[26037212 44395058 39633496] [26352287 44959895 40072452]]
                         25)
  (find-better-rectangle nanobots
                         [[26037212 44666174 39615938] [26301875 44982476 40090004]]
                         25)
  ;; conv 903 [26164244 44919214 39767634]
  (let [distance 5
        factor 1]
    (loop [point [26048747 44894534 39676821]
           distance-to-origo 110620102]
      (println "looping with " point distance-to-origo)
      (let [best (->> (for [x (range (- (first point) (* factor distance)) (+ (first point) (inc (* factor distance))) factor)
                            y (range (- (second point) (* factor distance)) (+ (second point) (inc (* factor distance))) factor)
                            z (range (- (third point) (* factor distance)) (+ (third point) (inc (* factor distance))) factor)]
                        [x y z])
                      (reduce (fn [a p]
                                (conj a [p (find-number-of-nanobots-in-range nanobots p)]))
                              [])
                      (filter (fn [[_ n]]
                                (when (>= n 921) (println "found" n))
                                (>= n 920)))
                      (map (fn [[p n]]
                             [p n (manhattan-distance p [0 0 0])]))
                      (sort-by (juxt (comp - second) first))
                      (first))]
        (if (< (third best) distance-to-origo)
          (recur (first best) (third best))
          best))))

  (time (let [point [26049758 44903728 39668638]
              ; 110622124
              ; [26063758 44918728 39667638] 110640124
              ; [26063758 44918728 39667638] 110650124
              ; [26163858 44918828 39767638] 110850124
              ; [26163958 44918928 39767638] 110850324
              ; [26164058 44919028 39767638] 110850524
              ; [26164088 44919058 39767638] 110850724
              ; [26164108 44919078 39767638] 110850784
              distance 5
              factor 1000]
          (->> (for [x (range (- (first point) (* factor distance)) (+ (first point) (inc (* factor distance))) factor)
                     y (range (- (second point) (* factor distance)) (+ (second point) (inc (* factor distance))) factor)
                     z (range (- (third point) (* factor distance)) (+ (third point) (inc (* factor distance))) factor)]
                 [x y z])
               (reduce (fn [a p]
                         (conj a [p (find-number-of-nanobots-in-range nanobots p)]))
                       [])
               (filter (fn [[_ n]]
                         (when (>= n 907) (println "found 907" n))
                         (>= n 906)))
               (map (fn [[p n]]
                      [p n (manhattan-distance p [0 0 0])]))
               (sort-by (juxt second third))
               (reverse)
               )))

  (find-better-rectangle nanobots
                         [[26121900 44666174 39748672] [26174830 44931866 39995178]]
                         25)
  (find-better-rectangle nanobots
                         [[26126134 44676801 39738812] [26162123 44931849 40005032]]
                         25)
  (find-better-rectangle nanobots
                         [[26121900 44666174 39748672] [26174830 44931866 39995178]]
                         25)
  (find-better-rectangle nanobots
                         [[26121900 44666174 39748672] [26174830 44931866 39995178]]
                         25)
  (find-better-rectangle nanobots
                         [[26121900 44666174 39748672] [26174830 44931866 39995178]]
                         25)
  (find-better-rectangle nanobots
                         [[26121900 44666174 39748672] [26174830 44931866 39995178]]
                         25)
  ;(get-range-step [33194740 25578571 36327737] [70294642 54812121 70997207] 20)

  (find-better-rectangle nanobots
                         [[33194740 40195341 43261629]]
                         20)
  (+ 200144301 170854721)
  (+ 120589179 171746322)
  (+ 154354348 192340365)

  (->> nanobots
       (filter (fn [n] (in-range? (:position n) [27541247 45713197 39775917] (:radius n))))
       (count))

  (time (find-coordinates-in-range-of-the-largets-number-of-nanobots
          nanobots
          (find-bounding-rectangle nanobots)
          25))
  ; "Elapsed time: 59177.433375 msecs"
  ;=> TOO HIGH  111475938
  ;; 113030361
  ;; TOO HIGH 110851088
  ;; 110851086
  ;; 110851076
  ;; 110851056
  ;; 110851036 TIME 101683
  ;; 110620102

  )