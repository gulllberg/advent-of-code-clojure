(ns advent-of-code.year-2019.day-16
  (:require [ysera.test :refer [is= is is-not]]
            [advent-of-code.maths :refer [binomial]]))

(def input (slurp "src/advent_of_code/year_2019/inputs/day16.txt"))
(def test-input "12345678")
(def test-input-2 "03036732577212944063491565474664")

(defn parse-input
  [input]
  (->> (re-seq #"\d" input)
       (map read-string)))

(defn create-pattern
  [index]
  (-> (concat (repeat (inc index) 0)
              (repeat (inc index) 1)
              (repeat (inc index) 0)
              (repeat (inc index) -1))
      (cycle)
      (rest)))

(defn fft-element
  [numbers index]
  (as-> (create-pattern index) $
        (map * numbers $)
        (reduce + $)
        (rem $ 10)
        (abs $)))

(defn fft-phase
  {:test (fn []
           (is= (fft-phase (parse-input test-input))
                [4 8 2 2 6 1 5 8])
           (is= (fft-phase [4 8 2 2 6 1 5 8])
                [3 4 0 4 0 4 3 8])
           (is= (fft-phase [3 4 0 4 0 4 3 8])
                [0 3 4 1 5 5 1 8])
           (is= (fft-phase [0 3 4 1 5 5 1 8])
                [0 1 0 2 9 4 9 8]))}
  [numbers]
  (->> (range (count numbers))
       (map (fn [index]
              (fft-element numbers index)))))

(defn part-1
  {:test (fn []
           (is= (part-1 "80871224585914546619083218645595") [2 4 1 7 6 1 7 6])
           (is= (part-1 "19617804207202209144916044189917") [7 3 7 4 5 4 1 8])
           (is= (part-1 "69317163492948606335995924319873") [5 2 4 3 2 1 3 3]))}
  [input]
  (as-> (parse-input input) $
        (iterate fft-phase $)
        (nth $ 100)
        (take 8 $)))

;; Part 2
;; With the repeating nature of the pattern, and the fact that all the numbers in the message are in the second half,
;; each number will have a pattern with 0 for every number "to the left" of it, and one for itself and all numbers to the right.
;; I.e, the rightmost number is only made up by itself, the second rightmost number is itself plus the rightmost number and so on.
;; It turns out that after a certain number of iterations, the times you have added each of the original numbers follows binomial coefficients.

(defn get-factor
  [distance iterations]
  (-> (binomial (+ (dec iterations) distance) (dec iterations))
      (mod 10)))

(def get-factor-memoized (memoize get-factor))

(defn get-number-after-iterations
  {:test (fn []
           (is= (get-number-after-iterations [5 6 7 8] 1 0 10) 0)
           (is= (get-number-after-iterations [5 6 7 8] 1 1 10) 6)
           (is= (get-number-after-iterations [5 6 7 8] 1 2 10) 7)
           (is= (get-number-after-iterations [5 6 7 8] 1 3 10) 8))}
  [numbers numbers-repeat-factor index iterations]
  (-> (reduce (fn [a j]
                (+ a (* (nth numbers (mod j (count numbers))) (get-factor-memoized (- j index) iterations))))
              0
              (range index (* (count numbers) numbers-repeat-factor)))
      (mod 10)))

(defn get-message-offset
  {:test (fn []
           (is= (get-message-offset test-input-2) 303673))}
  [input]
  (->> (subs input 0 7)
       (re-find #"[1-9]\d+")
       (read-string)))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input-2) [8 4 4 6 2 0 2 6]))}
  [input]
  (let [numbers (parse-input input)
        message-offset (get-message-offset input)]
    (reduce (fn [a i]
              (println i a)
              (conj a (get-number-after-iterations numbers 10000 (+ message-offset i) 100)))
            []
            (range 8))))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 4742.204333 msecs"
  ;=> "22122816"

  (time (part-2 input))
  ;; "Elapsed time: 45572.839625 msecs"
  ;=> [4N 1N 4N 0N 2N 1N 7N 1N]
  )
