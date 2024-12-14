(ns advent-of-code.year_2018.day_03)

(def input (slurp "src/advent_of_code/year_2018/inputs/day03.txt"))

(defn part-1
  []
  (let [claims (clojure.string/split-lines input)]
    (->> (reduce (fn [fabric claim]
                   (let [[offsets lengths] (map clojure.string/trim
                                                (-> claim
                                                    (clojure.string/split #"@")
                                                    (second)
                                                    (clojure.string/split #":")))
                         [x-offset y-offset] (map read-string (clojure.string/split offsets #","))
                         [x-length y-length] (map read-string (clojure.string/split lengths #"x"))]
                     (reduce (fn [fabric x-cord]
                               (reduce (fn [fabric y-cord]
                                         (update fabric [x-cord y-cord] (fn [n-claims]
                                                                          (if n-claims
                                                                            (inc n-claims)
                                                                            1))))
                                       fabric
                                       (range y-offset (+ y-offset y-length))))
                             fabric
                             (range x-offset (+ x-offset x-length)))))
                 {}
                 claims)
         (vals)
         (filter (fn [n-claims]
                   (>= n-claims 2)))
         (count))))

(comment
  (time (part-1))
  ;; 101781
  ;; "Elapsed time: 242.723709 msecs"
  )


(defn part-2
  []
  (let [claims (clojure.string/split-lines input)]
    (->> (reduce (fn [[candidates fabric] claim]
                   (let [id (-> claim
                                (clojure.string/split #"@")
                                (first))
                         [offsets lengths] (map clojure.string/trim
                                                (-> claim
                                                    (clojure.string/split #"@")
                                                    (second)
                                                    (clojure.string/split #":")))
                         [x-offset y-offset] (map read-string (clojure.string/split offsets #","))
                         [x-length y-length] (map read-string (clojure.string/split lengths #"x"))
                         [candidates fabric possible] (reduce (fn [[candidates fabric possible] x-cord]
                                                                (reduce (fn [[candidates fabric possible] y-cord]
                                                                          (let [old-claims (get fabric [x-cord y-cord] #{})
                                                                                n-old-claims (count old-claims)]
                                                                            [(if (= n-old-claims 0)
                                                                               candidates
                                                                               (clojure.set/difference candidates old-claims))
                                                                             (assoc fabric [x-cord y-cord] (conj old-claims id))
                                                                             (and possible (= n-old-claims 0))]))
                                                                        [candidates fabric possible]
                                                                        (range y-offset (+ y-offset y-length))))
                                                              [candidates fabric true]
                                                              (range x-offset (+ x-offset x-length)))]
                     [(if possible
                        (conj candidates id)
                        candidates)
                      fabric]))
                 [#{} {}]
                 claims)
         (first))))

(comment
  (time (part-2))
  ;; 909
  ;; "Elapsed time: 310.995208 msecs"
  )
