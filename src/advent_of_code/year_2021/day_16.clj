(ns advent-of-code.year-2021.day-16
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2021/inputs/day16.txt"))

(defn convert-char
  [hex-char]
  (condp = hex-char
    \0 "0000"
    \1 "0001"
    \2 "0010"
    \3 "0011"
    \4 "0100"
    \5 "0101"
    \6 "0110"
    \7 "0111"
    \8 "1000"
    \9 "1001"
    \A "1010"
    \B "1011"
    \C "1100"
    \D "1101"
    \E "1110"
    \F "1111"
    ""))

(defn hex->binary
  {:test (fn []
           (is= (hex->binary "D2FE28") "110100101111111000101000"))}
  [hex]
  (reduce (fn [binary-str hex-char]
            (str binary-str (convert-char hex-char)))
          ""
          hex))

;; https://stackoverflow.com/questions/5057047/how-to-do-exponentiation-in-clojure
(defn exp [x n]
  (reduce * (repeat n x)))

(defn binary-string->number
  {:test (fn []
           (is= (binary-string->number "10110") 22)
           (is= (binary-string->number "01001") 9))}
  [binary-string]
  (reduce-kv (fn [a i v]
               (+ a (* (read-string (str v)) (exp 2 i))))
             0
             (vec (reverse binary-string))))

(defn get-type-roughly
  [binary-string]
  (condp = (subs binary-string 0 3)
    "100" :literal
    :operator))

(defn parse-structure
  [binary-string]
  (loop [version-numbers 0
         remaining-string binary-string
         doing :version]
    (if (and (= doing :version)
             (< (count remaining-string) 11))
      version-numbers
      (condp = doing
        :version (recur (+ version-numbers (binary-string->number (subs remaining-string 0 3)))
                        (subs remaining-string 3)
                        :type)

        :type (recur version-numbers
                     (subs remaining-string 3)
                     (get-type-roughly (subs remaining-string 0 3)))

        :literal (if (= (first remaining-string) \0)
                   (recur version-numbers
                          (subs remaining-string 5)
                          :version)
                   (recur version-numbers
                          (subs remaining-string 5)
                          :literal))

        :operator (if (= (first remaining-string) \0)
                    (recur version-numbers
                           (subs remaining-string 1)
                           :operator-length)
                    (recur version-numbers
                           (subs remaining-string 1)
                           :operator-sub-packets))

        ;; For now just ignore the structure of sub packets - assume that it's possible to tell when they terminate just by looking at the packets themselves.
        :operator-length (recur version-numbers
                                (subs remaining-string 15)
                                :version)

        :operator-sub-packets (recur version-numbers
                                     (subs remaining-string 11)
                                     :version)))))

(defn solve-a
  []
  (parse-structure (hex->binary input)))

(comment
  (solve-a)
  ; 989
  )

(defn get-type
  [binary-string]
  (condp = (subs binary-string 0 3)
    "000" :sum
    "001" :product
    "010" :minimum
    "011" :maximum
    "100" :literal
    "101" :greater-than
    "110" :less-than
    "111" :equal-to))

(defn get-length-type
  [char]
  (if (= char \0)
    :length
    :sub-packets))

(defn read-packages
  {:test (fn []
           (is= (first (flatten (read-packages (hex->binary "C200B40A82") :sub-packets 1 []))) 3)
           (is= (first (flatten (read-packages (hex->binary "04005AC33890") :sub-packets 1 []))) 54)
           (is= (first (flatten (read-packages (hex->binary "880086C3E88112") :sub-packets 1 []))) 7)
           (is= (first (flatten (read-packages (hex->binary "CE00C43D881120") :sub-packets 1 []))) 9)
           (is= (first (flatten (read-packages (hex->binary "D8005AC2A8F0") :sub-packets 1 []))) 1)
           (is= (first (flatten (read-packages (hex->binary "F600BC2D8F") :sub-packets 1 []))) 0)
           (is= (first (flatten (read-packages (hex->binary "9C005AC2F8F0") :sub-packets 1 []))) 0)
           (is= (first (flatten (read-packages (hex->binary "9C0141080250320F1802104A08") :sub-packets 1 []))) 1))}
  [binary-string length-type length previous-result]
  (let [[result remaining-string] (loop [remaining-string binary-string
                                         doing :version
                                         literal-result ""]
                                    (condp = doing
                                      :version (recur (subs remaining-string 3)
                                                      :type
                                                      literal-result)

                                      :type (recur (subs remaining-string 3)
                                                   (get-type (subs remaining-string 0 3))
                                                   literal-result)

                                      :literal (if (= (first remaining-string) \0)
                                                 ;; last part of literal
                                                 [(binary-string->number (str literal-result (subs remaining-string 1 5)))
                                                  (subs remaining-string 5)]
                                                 ;; not last part of literal
                                                 (recur (subs remaining-string 5)
                                                        :literal
                                                        (str literal-result (subs remaining-string 1 5))))

                                      (let [length-type (get-length-type (first remaining-string))
                                            num-bits-for-length (if (= length-type :length) 15 11)
                                            [result new-remaining-string] (read-packages (subs remaining-string (inc num-bits-for-length))
                                                                                         length-type
                                                                                         (binary-string->number (subs remaining-string 1 (inc num-bits-for-length)))
                                                                                         [])]
                                        (condp = doing
                                          :sum [(apply + result)
                                                new-remaining-string]

                                          :product [(apply * result)
                                                    new-remaining-string]

                                          :minimum [(apply min result)
                                                    new-remaining-string]

                                          :maximum [(apply max result)
                                                    new-remaining-string]

                                          :greater-than [(if (apply > result) 1 0)
                                                         new-remaining-string]

                                          :less-than [(if (apply < result) 1 0)
                                                      new-remaining-string]

                                          :equal-to [(if (apply = result) 1 0)
                                                     new-remaining-string]))))]
    (if (= length-type :length)
      ;; :length
      (let [remaining-length (- length (- (count binary-string) (count remaining-string)))]
        (if (< remaining-length 11)
          ;; not enough left for another package, return
          [(conj previous-result result)
           remaining-string]
          ;; more string left
          (read-packages remaining-string
                         length-type
                         remaining-length
                         (conj previous-result result))))
      ;; :sub-packets
      (if (= length 1)
        ;; last packet, return
        [(conj previous-result result)
         remaining-string]
        ;; packets remain
        (read-packages remaining-string
                       length-type
                       (dec length)
                       (conj previous-result result))))))

(defn solve-b
  []
  (first (flatten (read-packages (hex->binary input) :sub-packets 1 []))))

(comment
  (solve-b)
  ; 7936430475134
  )
