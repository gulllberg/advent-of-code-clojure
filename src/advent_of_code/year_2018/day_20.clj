(ns advent-of-code.year-2018.day-20
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2018/inputs/day20.txt"))
(def test-input "^WNE$")
(def test-input-2 "^ENWWW(NEEE|SSE(EE|N))$")
(def test-input-3 "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$")
(def test-input-4 "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$")
(def test-input-5 "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$")

(defn parse-input
  {:test (fn []
           (is= (parse-input test-input) '["WNE"])
           (is= (parse-input test-input-2) '["ENWWW" (["NEEE"] ["SSE" (["EE"] ["N"])])])
           (is= (parse-input test-input-3) '["ENNWSWW" (["NEWS"] []) "SSSEEN" (["WNSE"] []) "EE" (["SWEN"] []) "NNN"])
           (is= (parse-input test-input-4) '["ESSWWN" (["E"] ["NNENN" (["EESS" (["WNSE"] []) "SSS"] ["WWWSSSSE" (["SW"] ["NNNE"])])])])
           (is= (parse-input test-input-5) '["WSSEESWWWNW" (["S"] ["NENNEEEENN" (["ESSSSW" (["NWSW"] ["SSEN"])] ["WSWWN" (["E"] ["WWS" (["E"] ["SS"])])])])]))}
  [input]
  (-> input
      (clojure.string/replace "^" "[")
      (clojure.string/replace "$" "]")
      (clojure.string/replace "(" "([")
      (clojure.string/replace ")" "])")
      (clojure.string/replace "|" "][")
      (clojure.string/replace #"(\w+)" "\"$1\"")
      (clojure.edn/read-string)))

(def c->direction {\N [0 1]
                   \S [0 -1]
                   \E [1 0]
                   \W [-1 0]})

(defn handle-plain-instructions-for-position
  [grid position instructions]
  (reduce (fn [[grid position] c]
            (let [direction (c->direction c)
                  new-position (mapv + position direction)]
              [(-> grid
                   (update position (fnil conj #{}) new-position)
                   (update new-position (fnil conj #{}) position))
               new-position]))
          [grid position]
          (sequence instructions)))

(defn handle-plain-instructions
  [grid positions instructions]
  (reduce (fn [[grid positions] position]
            (let [[grid position] (handle-plain-instructions-for-position grid position instructions)]
              [grid (conj positions position)]))
          [grid #{}]
          positions))

(defn handle-instructions
  [grid positions instructions]
  (cond
    (vector? instructions)
    (reduce (fn [[grid positions] instructions]
              (handle-instructions grid positions instructions))
            [grid positions]
            instructions)

    (seq? instructions)
    (reduce (fn [[grid new-positions] instructions]
              (let [[grid new-positions-2] (handle-instructions grid positions instructions)]
                [grid (clojure.set/union new-positions new-positions-2)]))
            [grid #{}]
            instructions)

    :else
    (handle-plain-instructions grid positions instructions)))

(defn find-longest-path
  [grid]
  (loop [visited #{[0 0]}
         positions #{[0 0]}
         steps 0]
    (let [[visited positions] (reduce (fn [[visited positions] position]
                                        [(clojure.set/union visited (get grid position))
                                         (clojure.set/union positions (clojure.set/difference (get grid position) visited))])
                                      [visited #{}]
                                      positions)]
      (if (empty? positions)
        steps
        (recur visited positions (inc steps))))))

(defn find-paths-over-1000-steps
  [grid]
  (loop [visited #{[0 0]}
         positions #{[0 0]}
         steps 0]
    (if (= steps 999)
      (- (count (keys grid)) (count visited))
      (let [[visited positions] (reduce (fn [[visited positions] position]
                                          [(clojure.set/union visited (get grid position))
                                           (clojure.set/union positions (clojure.set/difference (get grid position) visited))])
                                        [visited #{}]
                                        positions)]
        (recur visited positions (inc steps))))))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 3)
           (is= (part-1 test-input-2) 10)
           (is= (part-1 test-input-3) 18)
           (is= (part-1 test-input-4) 23)
           (is= (part-1 test-input-5) 31))}
  [input]
  (->> (parse-input input)
       (handle-instructions {[0 0] #{}} #{[0 0]})
       (first)
       (find-longest-path)))

(defn part-2
  [input]
  (->> (parse-input input)
       (handle-instructions {[0 0] #{}} #{[0 0]})
       (first)
       (find-paths-over-1000-steps)))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 41.766042 msecs"
  ;=> 4186

  (time (part-2 input))
  ;; "Elapsed time: 13.941958 msecs"
  ;=> 8466
  )
