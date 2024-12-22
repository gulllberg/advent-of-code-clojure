(ns advent-of-code.year-2024.day-22
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2024/inputs/day22.txt"))
(def test-input "1\n10\n100\n2024")

(defn mix
  {:test (fn []
           (is= (mix 42 15)
                37))}
  [n1 n2]
  (bit-xor n1 n2))

(defn prune
  {:test (fn []
           (is= (prune 100000000)
                16113920))}
  [n]
  (mod n 16777216))

(defn evolve
  {:test (fn []
           (is= (evolve 123) 15887950)
           (is= ((apply comp (repeat 2 evolve)) 123) 16495136)
           (is= (as-> (range 1 11) $
                      (map (fn [n] (apply comp (repeat n evolve))) $)
                      ((apply juxt $) 123))
                [15887950 16495136 527345 704524 1553684 12683156 11100544 12249484 7753432 5908254]))
   }
  [secret]
  (let [secret (-> secret
                   (* 64)
                   (mix secret)
                   (prune))
        secret (-> secret
                   (/ 32)
                   (int)
                   (mix secret)
                   (prune))]
    (-> secret
        (* 2048)
        (mix secret)
        (prune))))

(defn evolve-times
  {:test (fn []
           (is= (evolve-times 1 2000)
                8685429)
           (is= (evolve-times 10 2000)
                4700978))}
  [secret n]
  ((apply comp (repeat n evolve)) secret))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 37327623))}
  [input]
  (->> (clojure.string/split-lines input)
       (map clojure.edn/read-string)
       (map (fn [secret]
              (evolve-times secret 2000)))
       (reduce +)))

(defn get-price
  {:test (fn []
           (is= (get-price 16495136) 6))}
  [secret]
  (mod secret 10))

(defn get-changes->price-for-secret
  {:test (fn []
           (is= (-> (get-changes->price-for-secret 1)
                    (get [-2 1 -1 3]))
                7)
           (is= (-> (get-changes->price-for-secret 2)
                    (get [-2 1 -1 3]))
                7)
           (is= (-> (get-changes->price-for-secret 3)
                    (get [-2 1 -1 3]))
                nil)
           (is= (-> (get-changes->price-for-secret 2024)
                    (get [-2 1 -1 3]))
                9))}
  [secret]
  (loop [n 0
         secret secret
         prices [(get-price secret)]
         change-sequence []
         result {}]
    (if (= n 2000)
      result
      (let [new-secret (evolve secret)
            new-secret-price (get-price new-secret)
            change (- new-secret-price (last prices))
            change-sequence (->> (conj change-sequence change)
                                 (take-last 4)
                                 (into []))]
        (recur (inc n)
               new-secret
               (conj prices new-secret-price)
               change-sequence
               (if (or (contains? result change-sequence)
                       (< (count change-sequence) 4))
                 result
                 (assoc result change-sequence new-secret-price)))))))

(def test-input-2 "1\n2\n3\n2024")

(defn part-2
  {:test (fn []
           (is= (part-2 test-input-2) 23))}
  [input]
  (let [all-changes->price  (->> (clojure.string/split-lines input)
                                 (map clojure.edn/read-string)
                                 (map get-changes->price-for-secret))]
    (->> all-changes->price
         (map keys)
         (reduce (fn [a ks]
                   (reduce conj a ks))
                 #{})
         (map (fn [change-sequence]
                (reduce (fn [a change->price]
                          (+ a (get change->price change-sequence 0)))
                        0
                        all-changes->price)))
         (apply max))))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 674.832167 msecs"
  ;=> 19877757850

  (time (part-2 input))
  ;; "Elapsed time: 80763.46425 msecs"
  ;=> 2399
  )
