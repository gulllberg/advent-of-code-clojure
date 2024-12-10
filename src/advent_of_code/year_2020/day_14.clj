(ns advent-of-code.year-2020.day-14
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2020/inputs/day14.txt"))

(defn parse-line
  {:test (fn []
           (is= (parse-line "mask = 00010010001010101XXXX000000X111X0111") "00010010001010101XXXX000000X111X0111")
           (is= (parse-line "mem[32507] = 5127835") ["32507" "5127835"]))}
  [line]
  (if-let [matches (re-matches #"mask = (.*)" line)]
    (second matches)
    (subvec (re-matches #"mem\[(.*)\] = (.*)" line) 1 3)))

(defn binary-string->base10
  {:test (fn []
           (is= (binary-string->base10 "1000110") 70)
           (is= (binary-string->base10 "111") 7)
           (is= (binary-string->base10 "000000000000000000000000000001001001") 73)
           (is= (binary-string->base10 "000100100010101011100000000011110111") 4876796151))}
  [binary-string]
  (long (reduce (fn [a i]
                 (if (= (nth (reverse binary-string) i) \1)
                   (+ a (Math/pow 2 i))
                   a))
               0
               (range (count binary-string)))))

(defn base10->binary-string
  {:test (fn []
           (is= (base10->binary-string 70) "1000110")
           (is= (base10->binary-string 7) "111"))}
  [base10]
  (Integer/toString base10 2))

(defn left-pad
  {:test (fn []
           (is= (left-pad "101" 5) "00101")
           (is= (left-pad "101" 2) "101")
           (is= (left-pad "101" 3) "101"))}
  [string wanted-length]
  (if (>= (count string) wanted-length)
    string
    (str (apply str (repeat (- wanted-length (count string)) "0")) string)))

(defn replace-at
  {:test (fn []
           (is= (replace-at "00101" 1 1) "01101"))}
  [string i replacement]
  (str (subs string 0 i) replacement (subs string (inc i))))

(defn apply-mask
  {:test (fn []
           (is= (apply-mask "1011" "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X") "000000000000000000000000000001001001"))}
  [binary-string mask]
  (reduce (fn [s i]
            (if (= (nth mask i) \X)
              s
              (replace-at s i (nth mask i))))
          (left-pad binary-string (count mask))
          (range (count mask))))

(defn solve-a
  []
  (apply + (vals (first (reduce (fn [[memory mask] line]
                                  (if-let [matches (re-matches #"mask = (.*)" line)]
                                    [memory (second matches)]
                                    (let [[_ mem value] (re-matches #"mem\[(.*)\] = (.*)" line)]
                                      [(assoc memory mem (binary-string->base10 (apply-mask (base10->binary-string (read-string value)) mask))) mask])))
                                [{} nil]
                                (clojure.string/split-lines input))))))

(comment
  (solve-a)
  ; 3059488894985
  )

(defn apply-mask-b
  {:test (fn []
           (is= (apply-mask-b "101010" "000000000000000000000000000000X1001X") "000000000000000000000000000000X1101X"))}
  [binary-string mask]
  (reduce (fn [s i]
            (if (= (nth mask i) \0)
              s
              (replace-at s i (nth mask i))))
          (left-pad binary-string (count mask))
          (range (count mask))))

(defn get-all-addresses
  {:test (fn []
           (is= (get-all-addresses "00000000000000000000000000000001X0XX")
                #{"000000000000000000000000000000010000"
                  "000000000000000000000000000000010001"
                  "000000000000000000000000000000010010"
                  "000000000000000000000000000000010011"
                  "000000000000000000000000000000011000"
                  "000000000000000000000000000000011001"
                  "000000000000000000000000000000011010"
                  "000000000000000000000000000000011011"}))}
  [address]
  (reduce (fn [a i]
            (if (= (nth address i) \X)
              (reduced (clojure.set/union a (get-all-addresses (replace-at address i "1")) (get-all-addresses (replace-at address i "0"))))
              (if (= i (dec (count address)))
                (conj a address)
                a)))
          #{}
          (range (count address))))

(defn solve-b
  []
  (apply + (vals (first (reduce (fn [[memory mask] line]
                                  (if-let [matches (re-matches #"mask = (.*)" line)]
                                    [memory (second matches)]
                                    (let [[_ mem value] (re-matches #"mem\[(.*)\] = (.*)" line)]
                                      [(reduce (fn [m address]
                                                 (assoc m address (read-string value)))
                                               memory
                                               (get-all-addresses (apply-mask-b (base10->binary-string (read-string mem)) mask)))
                                       mask])))
                                [{} nil]
                                (clojure.string/split-lines input))))))

(comment
  (solve-b)
  ; 2900994392308
  )
