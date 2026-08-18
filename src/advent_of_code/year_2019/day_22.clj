(ns advent-of-code.year-2019.day-22
  (:require [ysera.test :refer [is= is is-not]]
            [advent-of-code.maths :refer [mod-inverse mod-pow]]))

(def input (slurp "src/advent_of_code/year_2019/inputs/day22.txt"))
(def test-input "deal into new stack\ncut -2\ndeal with increment 7\ncut 8\ncut -4\ndeal with increment 7\ncut 3\ndeal with increment 9\ndeal with increment 3\ncut -1")

(defn keywordize-command
  [command]
  (condp = command
    "deal into new stack" :deal-stack
    "cut" :cut
    "deal with increment" :deal-increment))

(defn parse-input
  [input]
  (->> (clojure.string/split-lines input)
       (map (fn [line]
              (let [command (keywordize-command (re-find #"deal into new stack|cut|deal with increment" line))
                    number (read-string (or (re-find #"-?\d+" line) "nil"))]
                [command number])))))

(defn convert-result-for-testing
  [result]
  (->> result
       (map-indexed (fn [index n]
                      [index n]))
       (reduce (fn [a [index n]]
                 (assoc a n index))
               (into [] (range (count result))))))

(defn deal-stack
  {:test (fn []
           (is= (deal-stack 10 3) 6)
           (is= (->> 3
                     (deal-stack 10)
                     (deal-stack 10))
                3))}
  [deck-size position]
  (mod (- 0 position 1) deck-size))

(defn deal-increment
  {:test (fn []
           (is= (deal-increment 10 3 3) 9)
           (is= (deal-increment 10 8 3) 4)
           (is= (->> (range 10)
                     (map (fn [position]
                            (deal-increment 10 position 7)))
                     (convert-result-for-testing))
                [0 3 6 9 2 5 8 1 4 7]))}
  [deck-size position increment]
  (mod (* position increment) deck-size))

(defn cut
  {:test (fn []
           (is= (cut 10 3 3) 0)
           (is= (cut 10 1 3) 8)
           (is= (cut 10 1 -4) 5)
           (is= (cut 10 8 -4) 2))}
  [deck-size position n]
  (mod (- position n) deck-size))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input 10 8) 3)
           (is= (->> (range 10)
                     (map (fn [position]
                            (part-1 test-input 10 position)))
                     (convert-result-for-testing))
                [9 2 5 8 1 4 7 0 3 6]))}
  [input deck-size starting-position]
  (->> (parse-input input)
       (reduce (fn [position [command number]]
                 (condp = command
                   :deal-stack (deal-stack deck-size position)
                   :cut (cut deck-size position number)
                   :deal-increment (deal-increment deck-size position number)))
               starting-position)))

;; Treat each operation as a linear transformation, y = k * x + m
;; For starting position x and ending position y

(defn deal-stack-2
  [deck-size [k m]]
  [(mod (- k) deck-size) (mod (- 0 m 1) deck-size)])

(defn deal-increment-2
  [deck-size [k m] increment]
  [(mod (* k increment) deck-size) (mod (* m increment) deck-size)])

(defn cut-2
  [deck-size [k m] n]
  [k (mod (- m n) deck-size)])

(defn do-instructions
  [deck-size instructions]
  (reduce (fn [position [command number]]
            (condp = command
              :deal-stack (deal-stack-2 deck-size position)
              :cut (cut-2 deck-size position number)
              :deal-increment (deal-increment-2 deck-size position number)))
          [1 0]
          instructions))

;; Applying y = k * x + m, t times (modulo n)
;; y = k^t * x + m * (k^t - 1) * (k - 1)^-1 (modulo n, where ^-1 is multiplicative inverse)
;; Denoting m * (k^t - 1) * (k - 1)^-1 as M
;; y = k^t * x + M (modulo n)
(defn do-instructions-times
  [deck-size instructions times]
  (let [[k m] (do-instructions deck-size instructions)
        kt (mod-pow k times deck-size)
        M (mod (* m (- kt 1) (mod-inverse (- k 1) deck-size)) deck-size)]
    [kt M]))

;; We know y and want to find x, so
;; x = (y - M) * (k^t)^-1
;; Which has been simplified above to the form
;; y = k * x + m (with different k and m)
(defn get-starting-position
  {:test (fn []
           (is= (get-starting-position 7 [5 0] 3) 2))}
  [deck-size [k m] position]
  (let [inverse (mod-inverse k deck-size)]
    (mod (* (- position m) inverse) deck-size)))

(defn part-2
  [input]
  (let [deck-size 119315717514047
        times 101741582076661
        instructions (parse-input input)
        [k m] (do-instructions-times deck-size instructions times)]
    (get-starting-position deck-size [k m] 2020)))

(comment
  (time (part-1 input 10007 2019))
  ;; "Elapsed time: 0.450834 msecs"
  ;=> 7096

  (time (part-2 input))
  ;; "Elapsed time: 1.588667 msecs"
  ;=> 27697279941366N
  )